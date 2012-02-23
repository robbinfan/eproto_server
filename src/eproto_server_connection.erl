
-module(eproto_server_connection).

-export([start_link/0, init/1]).

-include("msg_pb.hrl").

-record(v1, {parent, sock, callback, buf, buf_len, recv_len, connection_state, pending_recv}).

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [self()])}.

init(Parent) ->
    receive
        {go, Sock, SockTransform} ->
            start_connection(Parent, Sock, SockTransform)
    end.

switch_callback(State, Callback, Length) ->
    State#v1{callback=Callback, recv_len  = Length}.

socket_op(Sock, Fun) ->
    case Fun(Sock) of
        {ok, Res}       -> Res;
        {error, Reason} -> error_logger:error_msg("error on TCP connection ~p:~p~n",
                                                  [self(), Reason]),
                           error_logger:info_msg("closing TCP connection ~p~n",
                                                 [self()]),
                           exit(normal)
    end.

start_connection(Parent, Sock, SockTransform) ->
    process_flag(trap_exit, true),
    {PeerAddress, PeerPort} = socket_op(Sock, fun tcp_server_util:peername/1),
    PeerAddressS = tcp_server_util:ntoab(PeerAddress),
    
    ClientSock = socket_op(Sock, SockTransform),
    %erlang:send_after(?HANDSHAKE_TIMEOUT * 1000, self(), handshake_timeout),
    
    State = #v1{parent = Parent,
                sock   = ClientSock,
                callback = uninitialized_callback,
                recv_len = 0,
                pending_recv = false,
                connection_state = pre_init,
                buf = [],
                buf_len = 0},
    try
        recvloop(switch_callback(State, handshake, 12))
    catch
        Ex -> error_logger:error_msg(
                 "exception on TCP connection ~p from ~s:~p~n~p~n",                    
                 [self(), PeerAddressS, PeerPort, Ex])
    after
        error_logger:error_msg("closing TCP connection ~p from ~s:~p~n",
                               [self(), PeerAddressS, PeerPort])
    end,
    done.

recvloop(State = #v1{pending_recv = true}) ->
    mainloop(State);

recvloop(State = #v1{connection_state = blocked}) ->
    mainloop(State);

recvloop(State = #v1{sock = Sock, recv_len = RecvLen, buf_len = BufLen}) 
    when BufLen < RecvLen ->
        ok = tcp_server_util:setopts(Sock, [{active, once}]),
        mainloop(State#v1{pending_recv = true});

recvloop(State = #v1{buf = Buf, recv_len = RecvLen, buf_len = BufLen}) ->
    {Data, Rest} = split_binary(case Buf of
                                     [B] -> B;
                                     _ -> list_to_binary(lists:reverse(Buf))
                                end, RecvLen),
    recvloop(handle_input(State#v1.callback, Data,
                          State#v1{buf = [Rest],
                                   buf_len = BufLen - RecvLen})).

mainloop(State = #v1{sock = Sock, buf = Buf, buf_len = BufLen}) ->
    case tcp_server_util:recv(Sock) of
        {data, Data} -> recvloop(State#v1{buf = [Data | Buf],
                                          buf_len = BufLen + size(Data),
                                          pending_recv = false});
        close -> if State#v1.connection_state =:= closed ->
                        State;
                    true ->
                        throw(connection_closed_abruptly)
                end;
        {error, Reason} -> throw({inet_error, Reason});
        {other, Other} -> handle_other(Other, State)
    end.

handle_other(_Other, _State) ->
    error_logger:error_msg("recv other msg~n").

handle_input(handshake, <<_Id:64, _Cmd:16, Length:16>>, State) ->
    switch_callback(State, pkg_body, Length);

handle_input(pkg_body, Data, State) ->
    Result = msg_pb:decode_collecttweet(Data),
    lists:foreach(fun(I) -> eproto_server_storage:add_record({Result#collecttweet.uin, I#collecttweetinfo.tweet_id}) end, Result#collecttweet.collect_tweet_info),
    recvloop(switch_callback(State, handshake, 12)).

