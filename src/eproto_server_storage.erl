-module(eproto_server_storage).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add_record/1]).

-record(state, {num}).

%%----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------

init([]) ->
    ets:new(?MODULE, [duplicate_bag, public, named_table]),
    {ok, #state{num = 0}}.

handle_call({add_record, Record}, _From, State = #state{num = Num}) -> 
    ets:insert(?MODULE, Record),
    {reply, State#state{num = Num + 1}, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason,_State) ->
    ets:delete(?MODULE).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_record(Record) ->
    gen_server:call(?MODULE, {add_record, Record}).


