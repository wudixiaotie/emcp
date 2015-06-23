-module (emcp_config).

-export ([pool_size/0, hpd/0, host/0, port/0, db/0, timeout/0]).

-define (PoolSize, 10).
-define (Host, <<"localhost">>).
-define (Port, 27017).
-define (Db, <<"test">>).
-define (Timeout, 2000).


%% ===================================================================
%% Apis
%% ===================================================================

pool_size () ->
    get (pool_size, ?PoolSize).

hpd () ->
    {host (), port (), db ()}.

host () ->
    get (host, ?Host).

port () ->
    get (port, ?Port).

db () ->
    get (db, ?Db).

timeout () ->
    get (timeout, ?Timeout).



%% ===================================================================
%% Internal functions
%% ===================================================================

get (Key, Default) ->
    case application:get_env (emcp, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.