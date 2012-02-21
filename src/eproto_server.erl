
-module(eproto_server).

-export([start/0]).
-export([start_client/1, start_client/2]).
-export([tcp_listener_started/2, tcp_listener_stopped/2]).


start() ->
    start_listener(),
    start_server().

start_listener() ->
    TcpOptions = [binary,
        {packet, raw},
        {reuseaddr, true},
        {backlog, 128},
        {nodelay, true},
        {exit_on_close, true}],
    [{IPAddress, Port, _, Label}] = tcp_server_util:check_tcp_listener_address(
        tcp, {"127.0.0.1", 23001, inet}),
    {ok, _} = supervisor:start_child(
        tcp_server_sup,
        [IPAddress, Port, TcpOptions,
            {eproto_server, tcp_listener_started, []},
            {eproto_server, tcp_listener_stopped, []},
            {eproto_server, start_client, []},
            Label]).

start_server() ->
    eproto_server_sup:start_link({eproto_server_connection, start_link, []}).

start_client(Sock, SockTransform) ->
    {ok, Child} = supervisor:start_child(eproto_server_sup, []),
    ok = gen_tcp:controlling_process(Sock, Child),
    Child ! {go, Sock, SockTransform},
    Child.

start_client(Sock) ->
    start_client(Sock, fun (S) -> {ok, S} end).


tcp_listener_started(IPAddress, Port) ->
    io:format("[~p:~p] listerner started~n", [IPAddress, Port]).

tcp_listener_stopped(IPAddress, Port) ->
    io:format("[~p:~p] listerner stopped~n", [IPAddress, Port]).
