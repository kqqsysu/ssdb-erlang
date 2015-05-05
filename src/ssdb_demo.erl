%%% @author kqqsysu@gmail.com
%%% @copyright (C) 2015, kongqingquan
%%% @doc  SSDB erlang client
%%% @end
%%% Created : 2015.05.05
%%%-------------------------------------------------------------------

-module(ssdb_demo).
-author("kqqsysu@gmail.com").

-include("ssdb.hrl").

-export([test/0,test/2,test/5]).

test() ->
    test("localhost",8888).
test(Host,Port) ->
    PoolSize = 5,
    Password = undefind,
    IsReconnect = 1,
    test(Host,Port,PoolSize,Password,IsReconnect).


test(Host,Port,PoolSize,Password,IsReconnect) ->
    %% connect to ssdb server
    ssdb:start(Host,Port,PoolSize,Password,IsReconnect),

    %% Key Value
    {ok,1} = ssdb:query([del,<<"ssdb_get_key">>]),
    {not_found,null} = ssdb:query([get,<<"ssdb_get_key">>]),
    {ok,1} = ssdb:query([set,<<"ssdb_get_key">>,<<"ssdb_set_value">>]),
    {ok,<<"ssdb_set_value">>} = ssdb:query([get,<<"ssdb_get_key">>]),

    %% Hashmap
    {ok,_} = ssdb:query([hdel,h,<<"ssdb_hash_key">>]),
    {not_found,null} = ssdb:query([hget,h,<<"ssdb_hash_key">>]),
    {ok,_} = ssdb:query([hset,h,<<"ssdb_hash_key">>,<<"ssdb_hashset_value">>]),
    {ok,<<"ssdb_hashset_value">>} = ssdb:query([hget,h,<<"ssdb_hash_key">>]),


    %% Sorted Set
    {ok,_} = ssdb:query([zdel,z,<<"ssdb_zset_key">>]),
    {not_found,null} = ssdb:query([zget,z,<<"ssdb_zset_key">>]),
    {ok,_} = ssdb:query([zset,z,<<"ssdb_zset_key">>,100]),
    {ok,100} = ssdb:query([zget,z,<<"ssdb_zset_key">>]),


    %% List
    {ok,_} = ssdb:query([qpush_front,q,<<"ssdb_qlist_value1">>,<<"ssdb_qlist_value2">>]),
    {ok,[<<"ssdb_qlist_value2">>,<<"ssdb_qlist_value1">>]} = ssdb:query([qpop_front,q,2]),

    ok.
