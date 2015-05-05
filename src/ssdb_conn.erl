%%% @author kqqsysu@gmail.com
%%% @copyright (C) 2015, kongqingquan
%%% @doc  SSDB erlang client.
%%% @end
%%% Created : 2015.05.01
%%%-------------------------------------------------------------------

-module(ssdb_conn).
-author("kqqsysu@gmail.com").

-include("ssdb.hrl").

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).

-export([start_link/2]).

start_link(Host,Port) ->
    gen_server:start_link(?MODULE,[Host,Port],[]).

init([Host,Port]) ->
    case gen_tcp:connect(Host, Port, ?SSDB_TCP_OPTS) of
        {ok, Socket} ->
            State = #ssdb_conn{socket = Socket,step = ?SSDB_STEP_FINISH},
            {ok,State};
        Error ->
            ?PRINT("Connect Errror:~p",[Error]),
            {stop,Error}
    end.

handle_cast(Msg,State)->
    ?PRINT("Not handle msg:~p",[Msg]),
    {noreply,State}.

handle_call({ssdb_query,Cmd},From,State = #ssdb_conn{from_list = FromList})->
    NewState = State#ssdb_conn{from_list = FromList ++ [{From,Cmd}]},
    NewState2 = send_query(NewState),
    {noreply,NewState2};

handle_call(Msg,From,State)->
    ?PRINT("Not handle msg from:~p ~p",[From,Msg]),
    {reply,error,State}.

handle_info({ssdb_query,From,Cmd},State = #ssdb_conn{from_list = FromList})->
    NewState = State#ssdb_conn{from_list = FromList ++ [{From,Cmd}]},
    NewState2 = send_query(NewState),
    {noreply,NewState2};

handle_info({tcp,Socket,Data},State = #ssdb_conn{socket = Socket})->
    case parse_recv(State,Data) of
        #ssdb_conn{step = ?SSDB_STEP_FINISH,reply = Reply,from_list = [{From,_Cmd} | FromList2]} = NewState ->
            gen_server:reply(From,Reply),
            NewState2 = NewState#ssdb_conn{from_list = FromList2,reply = []},
            NewState3 = send_query(NewState2),
            {noreply,NewState3};
        #ssdb_conn{} = NewState ->
            {noreply,NewState};
        error ->
            {stop,packet_error,State}
    end;
handle_info({tcp_closed,Socket},State = #ssdb_conn{socket = Socket})->
    ?PRINT("ssdb closed"),
    {stop,tcp_clised,State};

handle_info({tcp_error,Socket,Reason},State = #ssdb_conn{socket = Socket})->
    ?PRINT("ssdb socket error reason:~p",[Reason]),
    {stop,tcp_clised,State};

handle_info(Msg,State)->
    ?PRINT("Not handle msg:~p",[Msg]),
    {noreply,State}.

terminate(Reason,_State)->
    ?PRINT("trminate reason:~p",[Reason]),
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

%% parse packet
parse_recv(#ssdb_conn{step = ?SSDB_STEP_SIZE,data = Data} = State,InData) ->
    NewData = <<Data/binary,InData/binary>>,
    case binary:split(NewData,<<"\n">>) of
        [BinSize,BlockData] ->
            Size = erlang:binary_to_integer(BinSize),
            NewState = State#ssdb_conn{data = BlockData,size = Size,step = ?SSDB_STEP_DATA},
            parse_recv(NewState,<<>>);      %% parse data
        _ ->
            %% wait for get size
            State#ssdb_conn{data = NewData}
    end;
parse_recv(#ssdb_conn{step = ?SSDB_STEP_DATA,data = Data,size = Size,reply = Reply} = State,InData) ->
    NewData = <<Data/binary,InData/binary>>,
    case NewData of
        <<BlockData:Size/binary-unit:8,$\n:8,$\n:8,Res/binary>> ->
            case Res of
                <<>> ->
                    %% packet finish
                    NewReply = lists:reverse([BlockData | Reply]),
                    State#ssdb_conn{step = ?SSDB_STEP_FINISH,size = 0,data = <<>>,reply = NewReply};
                _ ->
                    ?PRINT("parse packet error"),
                    error
            end;
        <<BlockData:Size/binary-unit:8,$\n:8, Res/binary>> ->
            NewReply = [BlockData | Reply],
            NewState = State#ssdb_conn{step = ?SSDB_STEP_SIZE,size = 0,data = Res,reply = NewReply},
            parse_recv(NewState,<<>>);
        _ ->
            State#ssdb_conn{data = NewData}
    end.


%% send_packet
send_query(#ssdb_conn{step = ?SSDB_STEP_FINISH,socket = Socket, from_list = [{_From,Cmd} | _]} = State) ->
    Cmd2 = encode_cmd(Cmd,<<>>),
    gen_tcp:send(Socket,Cmd2),
    State#ssdb_conn{step = ?SSDB_STEP_SIZE};
send_query(#ssdb_conn{} = State) ->
    State.
    
encode_cmd([H | T],Cmd)->
    H2 = to_binary(H),
    Size = size(H2),
    Cmd2 = <<Cmd/binary, (to_binary(Size))/binary, $\n:8,H2/binary,$\n:8>>,
    encode_cmd(T,Cmd2);
encode_cmd([],Cmd) ->
    <<Cmd/binary, $\n:8>>.

to_binary(Msg) when is_binary(Msg) -> Msg;
to_binary(Msg) when is_atom(Msg) ->
    list_to_binary(atom_to_list(Msg));
to_binary(Msg) when is_list(Msg) -> 
    list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) ->
    integer_to_binary(Msg);
to_binary(Msg) when is_float(Msg) ->
    float_to_binary(Msg);
to_binary(Msg) when is_tuple(Msg) ->
    list_to_binary(tuple_to_list(Msg));
to_binary(_Msg) -> throw(other_value).

