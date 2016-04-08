# ssdb-client

Erlang SSDB Client. 

#How To Use

make sure ssdb-server is opening.

compile code use make

$ make

$ erl -pa ebin -pa deps/goldrush/ebin/ -pa deps/lager/ebin

1> ssdb:start("localhost",8888). 

{ok,<0.36.0>}

2> 
ssdb:query([set,a,<<"this is a test">>]).

{ok,1}

3> ssdb:query([get,a]).

{ok,<<"this is a test">>}

4> ssdb:query([hset,h,k,<<"test hset">>]).

{ok,1}

5> ssdb:query([hget,h,k]). 

{ok,<<"test hset">>}

more detail see src/ssdb_demo.erl

#Api
You can start ssdb client by ssdb:start/0,ssdb:start/2,ssdb:start/3,ssdb:start/4 or ssdb:start/5

ssdb:start(Host,Port,PoolSize,Password,IsReconnect)

		Host = list() default:"localhsot"
		
		Port = integer() default:8888
		
		PoolSize = integer() default:5
		
		Password = list() default:undefined
		
		IsReconned = 0/1 default 0 is auto reconnect to server when socket closed
		
		
use client api ssdb:query(Cmd),pass args by list

	Cmd = atom | ssdb_cmd()
	
	ssdb_cmd = list() eg. [get,a],[set,a,<<"abc">>],[hget,h,test] ...
