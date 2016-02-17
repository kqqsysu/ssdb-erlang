-define(PRINT(MSG), lager:info("~p ~w " ++ MSG ++ " ~n", [?MODULE,?LINE])).
-define(PRINT(MSG,INFO), lager:info("~p ~w " ++ MSG ++ " ~n", [?MODULE,?LINE] ++ INFO)).


-define(SSDB_TCP_OPTS,[binary,{active,true},{packet,0},{keepalive,true}]).

-define(SSDB_STEP_SIZE,0).
-define(SSDB_STEP_DATA,1).
-define(SSDB_STEP_FINISH,2).

-define(SSDB_RECONNECT,1).
-define(SSDB_RECONNECTING,1).

-define(SSDB_SERVER,ssdb_pool).
-define(SSDB_MAX_RECONNECT_TIME,(300 * 1000)).

-record(ssdb_conn,{
                    socket,
                    step = 0,
                    data = <<>>,
                    size = 0,            %% current data size
                    reply = [],          %% buffer of reply
                    from_list = []       %% FIFO query
                  }).


-record(ssdb_pool,{conn_pools = {[],[]},    %% {Unused Pools,Used Pools}
                   pool_size = 5,
                   host,
                   port,
                   password,
                   is_reconnect = 0,        %% 0/1
                   reconnect_time = 2,      %% seconds reconnection
                   is_reconnecting = 0  
                  }).
