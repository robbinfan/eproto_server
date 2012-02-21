
-module(tcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/7]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    TcpListenerSupChild = {tcp_listener_sup, 
        {tcp_listener_sup, start_link, []},
        temporary, brutal_kill, supervisor, [tcp_listener_sup]},
    {ok, {{simple_one_for_one, 0, 1}, [TcpListenerSupChild]}}.

start_child(IpAddress, Port, Options, OnStart, OnStop, AcceptCallback, Label) ->
    {ok, _} = supervisor:start_child(?MODULE, [IpAddress, Port, Options, OnStart, OnStop, AcceptCallback, Label]).
