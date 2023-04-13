-module(todus_pg).
-behaviour(gen_server).

-include("logger.hrl").

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2]).
-export([get_members/1,get_member/1,join/2,join/3]).

-record(state, {register, index}).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ?INFO_MSG("starting register",[]),
  {ok, #state{register = #{}, index = #{}}}.

handle_call({join, Group, PID, Metadata}, _From, #state{register = Register, index = Idx} = State) ->
  case maps:get(Group, Register, null) of
    null ->
      Register1 = Register#{Group => queue:from_list([#{pid => PID, meta => Metadata}])},
      Ref = monitor(process, PID),
      Idx1 = Idx#{Ref => Group},
      {reply, ok, State#state{register = Register1, index = Idx1}};
    Q ->
      Q1 = queue:in([#{pid => PID, meta => Metadata}], Q),
      Ref = monitor(process, PID),
      Register1 = Register#{Group => Q1},
      Idx1 = Idx#{Ref => Group},
      {reply, ok, State#state{register = Register1, index = Idx1}}
  end;
handle_call({get_members, Group}, _From, #state{register = Register} = State) ->
  case maps:get(Group, Register, null) of
    null ->
      {reply, {ok, []}, State};
    Q ->
      L = queue:to_list(Q),
      {reply, {ok, L}, State}
  end;
handle_call({get_member, Group}, _From, #state{register = Register} = State) ->
  case maps:get(Group, Register, null) of
    null ->
      {reply, undefined, State};
    Q ->
      {{value, C}, Q1} = queue:out(Q),
      Q2 = queue:in(C, Q1),
      NewQueue = Register#{Group => Q2},
      {reply, {ok, C}, State#state{register = NewQueue}}
  end;
handle_call(_Call, _From, State) ->
  {reply, ok, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid2, _Reason}, #state{register = Reg, index = Idx} = State) ->
  case maps:get(Ref, Idx, null) of
    null ->
      {noreply, State};
    G ->
      Idx1 = maps:remove(Ref, Idx),
      case maps:get(G, Reg, null) of
        null ->
          {noreply, State#state{index = Idx1}};
        Q ->
          F = fun(E) ->
                case maps:get(pid, E, null) of
                  null -> true;
                  P when P =:= _Pid2 ->
                    false;
                  P ->
                    true
                end
              end,
          Q1 = queue:filter(F, Q),
          {noreply, State#state{index = Idx1, register = Q1}}
      end
  end;
handle_info(Info, State) ->
  ?INFO_MSG("got info: ~p",[Info]),
  {noreply, State}.

terminate(Reason, _State) ->
  ?INFO_MSG("~p terminated, reason: ~p",[?MODULE,Reason]),
  ok.

-spec get_members( any() ) -> [].
get_members(Group) ->
  try gen_server:call(?MODULE, {get_members, Group}) of
    {ok, L} ->
      L
  catch
    E:R ->
      ?ERROR_MSG("getting client, (Error,Reason): (~p,~p)",[E,R]),
      []
  end.

-spec get_member( any() ) -> pid() | undefined.
get_member(Group) ->
  try gen_server:call(?MODULE, {get_member, Group}) of
    {ok, P} ->
      P;
    _R ->
      _R
  catch
    E:R ->
      ?ERROR_MSG("getting member (Error,Reason): (~p,~p)",[E,R]),
      undefined
  end.

-spec join( any(), pid() ) -> any().
join(Group, PID) ->
  join(Group, PID, #{}).

-spec join( any(), pid(), map() ) -> any().
join(Group, PID, Metadata) ->
  gen_server:call(?MODULE, {join, Group, PID, Metadata}).