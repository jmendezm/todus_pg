%%%-------------------------------------------------------------------
%% @doc todus_pg top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(todus_pg_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 100,
                 period => 5},
    PGSpec =
      #{id => todus_pg,
        start => {todus_pg, start, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [todus_pg]},
    ChildSpecs = [PGSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
