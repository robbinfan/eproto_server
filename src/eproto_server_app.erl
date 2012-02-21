-module(eproto_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tcp_server_sup:start_link(),
    eproto_server:start().

stop(_State) ->
    ok.
