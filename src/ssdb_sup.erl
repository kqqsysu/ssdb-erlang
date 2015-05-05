%%% @author kqqsysu@gmail.com
%%% @copyright (C) 2015, kongqingquan
%%% @doc  SSDB erlang client
%%% @end
%%% Created : 2015.05.01
%%%-------------------------------------------------------------------

-module(ssdb_sup).
-author("kqqsysu@gmail.com").

-include("ssdb.hrl").

-export([start_link/0,init/1,start_pool/5]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) -> 
	{ok, {{one_for_one, 3, 10}, []}}.

start_pool(Host,Port,PoolSize,Password,IsReconnect) ->
	supervisor:start_child(?MODULE,
                            {ssdb_pool, {ssdb_pool, start_link, [Host,Port,PoolSize,Password,IsReconnect]}, transient,
							5000, worker, [ssdb_pool]}).
