
-module(eproto_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link({M, F, A}) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [M, F, A]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([M, F, A]) ->
    {ok, {{simple_one_for_one, 0, 1}, [{M, {M, F, A}, temporary, brutal_kill, worker, [M]}]}}.

