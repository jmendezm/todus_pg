%%%-------------------------------------------------------------------
%% @doc todus_pg public API
%% @end
%%%-------------------------------------------------------------------

-module(todus_pg_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    todus_pg_logger:start(),
    todus_pg_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
