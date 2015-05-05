%%% @author kqqsysu@gmail.com
%%% @copyright (C) 2015, kongqingquan
%%% @doc  SSDB erlang client
%%% @end
%%% Created : 2015.05.01
%%%-------------------------------------------------------------------

-module(ssdb).
-author("kqqsysu@gmail.com").

-include("ssdb.hrl").

-export([start/0,start/2,start/3,start/4,start/5]).
-export([query/1,query/2]).




start()->
    start("localhost",8888).
start(Host,Port) ->
    PoolSize = 5,
    start(Host,Port,PoolSize).
start(Host,Port,PoolSize)->
    Password = undefined,
    start(Host,Port,PoolSize,Password).
start(Host,Port,PoolSize,Password)->
    IsReconnect = 0,
    start(Host,Port,PoolSize,Password,IsReconnect).
start(Host,Port,PoolSize,Password,IsReconnect) ->
    ssdb_sup:start_link(),
    ssdb_sup:start_pool(Host,Port,PoolSize,Password,IsReconnect).

query(Cmd) ->
    query(?SSDB_SERVER,Cmd).
query(Pid,[Cmd | _] = CmdList) ->
    Res = ssdb_pool:query(Pid,CmdList),
    AtomCmd = to_atom(Cmd),
    parse_res(AtomCmd,Res);
query(Pid,Cmd) ->
    ssdb_pool:query(Pid,[Cmd]).

parse_res(Cmd,Res) when Cmd == 'zavg' ->
    case Res of
        [<<"ok">>,Avg] ->
            try
                {ok,binary_to_float(Avg)}
            catch
                _:Exception ->
                    {error,Exception}
            end;
        _ ->
            ?PRINT("Query ~p Error:~p",[Cmd,Res]),
            Res
    end;
parse_res(Cmd,Res) when Cmd == 'get'; Cmd == 'substr'; Cmd == 'getset'; Cmd == 'hget'; Cmd == 'qget'; Cmd == 'qfront'; Cmd == 'qback' ->
    case Res of
        [<<"ok">>,Data] ->
            {ok,Data};
        [<<"not_found">>] ->
            {not_found,null};
        _ ->
            ?PRINT("Query ~p Error:~p",[Cmd,Res]),
            Res
    end;
parse_res(Cmd,Res) when Cmd == 'qpop';Cmd == 'qpop_front';Cmd == 'qpop_back' ->
    case Res of
        [<<"ok">> | Data] ->
            {ok,Data};
        [<<"not_found">>]->
            {not_found,null};
        _ ->
            ?PRINT("Query ~p Error:~p",[Cmd,Res]),
            Res
    end;
parse_res(Cmd,Res) when Cmd == 'keys';Cmd == 'zkeys';Cmd == 'hkeys';Cmd == 'hlist';Cmd == 'zlist';Cmd == 'qslice' ->
    case Res of
        [<<"ok">> | T] ->
            {ok,T};
        _ ->
            ?PRINT("Query ~p Error:~p",[Cmd,Res]),
            Res
    end;
parse_res(Cmd,Res) when Cmd == 'auth';Cmd == 'exists';Cmd == 'hexists';Cmd == 'zexists' ->
    case Res of
        [<<"ok">>,Data] ->
            try
                {ok,binary_to_integer(Data)}
            catch
                _:Exception ->
                    {error,Exception}
            end;
        _ ->
            ?PRINT("Query ~p Error:~p",[Cmd,Res]),
            Res
    end;

parse_res(Cmd,Res) when Cmd == 'multi_exists';Cmd == 'multi_hexists'; Cmd == 'multi_zexists'->
    case Res of
        [<<"ok">> | Data] ->
            try
                Return = parse_mutil_return(Data,[]),
                {ok,Return}
            catch
                _:Exception ->
                    {error,Exception}
            end;
        _ ->
            ?PRINT("Query ~p Error:~p",[Cmd,Res]),
            Res
    end;

parse_res(Cmd,Res) when 'scan';Cmd == 'rscan';Cmd == 'zscan';Cmd == 'zrscan';Cmd == 'zrange';Cmd == 'zrrange';Cmd == 'hscan';Cmd == 'hrscan';Cmd == 'hgetall';Cmd == 'multi_hsize';Cmd == 'multi_zsize';Cmd == 'multi_get';Cmd == 'multi_hget';Cmd == 'multi_zget';Cmd == 'zpop_front';Cmd == 'zpop_back' ->
    case Res of
        [<<"ok">> | Data] ->
            case atom_to_list(Cmd) of
                ["z" | _] ->
                    {ok,parse_mutil_return(Data,[])};
                _ ->
                    {ok,parse_mutil_return2(Data,[])}
            end;
        _ ->
            Res
    end;
%%% Cmd: 'dbsize','ping','qset','getbit','setbit','countbit','strlen','set','setx','setnx','zset','hset','qpush','qpush_front','qpush_back','qtrim_front','qtrim_back','del','zdel','hdel','hsize','zsize','qsize','hclear','zclear','qclear','multi_set','multi_del','multi_hset','multi_hdel','multi_zset','multi_zdel','incr','decr','zincr','zdecr','hincr','hdecr','zget','zrank','zrrank','zcount','zsum','zremrangebyrank','zremrangebyscore'
parse_res(Cmd,Res) ->
    case Res of
        [<<"ok">>] ->
            {ok,0};
        [<<"ok">>,Data] ->
            try
                {ok,binary_to_integer(Data)}
            catch
                _:Exception ->
                    {error,Exception}
            end;
        [<<"not_found">>]->
            {not_found,null};
        _ ->
            ?PRINT("Query ~p Error:~p",[Cmd,Res]),
            Res
    end.

parse_mutil_return([Key,IsExist | T],Res) ->
    NewRes = [{Key,binary_to_integer(IsExist)} | Res],
    parse_mutil_return(T,NewRes);
parse_mutil_return([],Res) ->
    lists:reverse(Res).

parse_mutil_return2([Key,Val | T],Res) ->
    NewRes = [{Key,Val} | Res],
    parse_mutil_return(T,NewRes);
parse_mutil_return2([],Res) ->
    lists:reverse(Res).



to_atom(Cmd) when is_atom(Cmd) ->
    Cmd;
to_atom(Cmd) when is_list(Cmd) ->
    list_to_atom(Cmd);
to_atom(Cmd) when is_binary(Cmd) ->
    binary_to_atom(Cmd,utf8).

