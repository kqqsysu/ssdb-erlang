%%% @author kqqsysu@gmail.com
%%% @copyright (C) 2015, kongqingquan
%%% @doc  SSDB erlang client
%%% @end
%%% Created : 2015.05.01
%%%-------------------------------------------------------------------

-module(ssdb_pool).
-author("kqqsysu@gmail.com").

-include("ssdb.hrl").

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).

-export([start_link/5,query/2]).

-define(SSDB_QUERY_TIMEOUT,(60 * 1000)).    %% 60 seconds

start_link(Host,Port,PoolSize,Password,IsReconnect)->
    gen_server:start_link({local,?SSDB_SERVER},?MODULE,[Host,Port,PoolSize,Password,IsReconnect],[]).

query(Pid,Cmd) ->
    gen_server:call(Pid,{ssdb_query,Cmd},?SSDB_QUERY_TIMEOUT).

init([Host,Port,PoolSize,Password,IsReconnect]) ->
    process_flag(trap_exit,true),
    Conns = start_pools(Host,Port,Password,PoolSize),
    State = #ssdb_pool{host = Host,port = Port,pool_size = PoolSize,is_reconnect = IsReconnect,conn_pools = {Conns,[]}},
    {ok,State}.

handle_cast(Msg,State)->
    ?PRINT("Not handle msg:~p",[Msg]),
    {noreply,State}.

handle_call({ssdb_query,Cmd},From,State) ->
    case get_conn(State) of
        {ok,Conn,NewState} ->
            Conn ! {ssdb_query,From,Cmd},
            {noreply,NewState};
        error ->
            {reply,error,State}
    end;

handle_call(Msg,From,State)->
    ?PRINT("Not handle msg from:~p ~p",[From,Msg]),
    {reply,error,State}.

handle_info({'EXIT', From, Reason},State = #ssdb_pool{conn_pools = {Unused,Used},is_reconnect = IsReconnect,is_reconnecting = IsReconnecting}) ->
    ?PRINT("ssdb_conn exit:From:~p,Reason:~p",[From,Reason]),
    NewUnused = lists:delete(From,Unused),
    NewUsed = lists:delete(From,Used),
    NewState = State#ssdb_pool{conn_pools = {NewUnused,NewUsed}},
    case IsReconnect == ?SSDB_RECONNECT andalso IsReconnecting /= ?SSDB_RECONNECTING of
        true ->
            self() ! reconnect;
        false ->
            ok
    end,
    {noreply,NewState};

handle_info(reconnect,State = #ssdb_pool{host = Host,port = Port,password = Password,reconnect_time = ReconnectTime,conn_pools = {Unused,Used},pool_size = PoolSize}) ->
    ConnectedSize = length(Unused) + length(Used),
    try
        NewConns = start_pools(Host,Port,Password,PoolSize - ConnectedSize),
        NewConnPools = {NewConns ++ Unused,Used},
        NewState = 
        case length(NewConns) + ConnectedSize >= PoolSize of
            true ->
                ?PRINT("reconnect success,poolsize:~w",[PoolSize]),
                State#ssdb_pool{conn_pools = NewConnPools,is_reconnecting = 0,reconnect_time = 2};
            false ->
                %% reconnect
                NewReconnectTime = min(?SSDB_MAX_RECONNECT_TIME,ReconnectTime * 2),
                erlang:send_after(NewReconnectTime * 1000,self(),reconnect),
                State#ssdb_pool{conn_pools = NewConnPools,is_reconnecting = ?SSDB_RECONNECTING,reconnect_time = NewReconnectTime}
        end,
        {noreply,NewState}
    catch
        _Err:_Exp ->
            ReconnectTime2 = min(?SSDB_MAX_RECONNECT_TIME,ReconnectTime * 2),
            erlang:send_after(ReconnectTime2 * 1000,self(),reconnect),
            {noreply,State#ssdb_pool{reconnect_time = ReconnectTime2,is_reconnecting = ?SSDB_RECONNECTING}}
    end;

handle_info(Msg,State)->
    ?PRINT("Not handle msg:~p",[Msg]),
    {noreply,State}.

terminate(Reason,_State)->
    ?PRINT("trminate reason:~p",[Reason]),
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

%% get next conn
get_conn(#ssdb_pool{conn_pools = {[],[]}} = _State) ->
    error;
get_conn(#ssdb_pool{conn_pools = {Unused,Used}} = State) ->
    case Unused of
        [] ->
            [Conn | Unused2] = lists:reverse(Used),
            NewState = State#ssdb_pool{conn_pools = {Unused2,[Conn]}},
            {ok, Conn,NewState};
        [Conn|Unused2] ->
            NewState = State#ssdb_pool{conn_pools = {Unused2,[Conn | Used]}},
            {ok,Conn,NewState}
    end.

start_pools(Host,Port,Password,PoolSize) ->
    lists:foldl(fun(_,AccIn) ->
                     {ok,Pid} = ssdb_conn:start_link(Host,Port),
                     case Password of
                         undefined ->
                             ok;
                         _ ->
                             [<<"ok">>,<<"1">>] = gen_server:call(Pid,{ssdb_query,[auth,Password]})
                     end,
                    [Pid | AccIn]
                end,[],lists:seq(1,PoolSize)).
