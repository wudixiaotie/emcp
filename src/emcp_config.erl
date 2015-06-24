-module (emcp_config).

-export ([pool_size/0, hpd/0, host/0, port/0, db/0, timeout/0, max_conn/0]).

-define (POOL_SIZE, 10).
-define (HOST, <<"localhost">>).
-define (PORT, 27017).
-define (DB, <<"test">>).
-define (TIMEOUT, 2000).
-define (MAX_CONN, 10).


%% ===================================================================
%% Apis
%% ===================================================================

pool_size () ->
    get (pool_size, ?POOL_SIZE).

hpd () ->
    {host (), port (), db ()}.

host () ->
    get (host, ?HOST).

port () ->
    get (port, ?PORT).

db () ->
    get (db, ?DB).

timeout () ->
    get (timeout, ?TIMEOUT).

max_conn () ->
    get (max_conn, ?MAX_CONN).



%% ===================================================================
%% Internal functions
%% ===================================================================

get (Key, Default) ->
    case application:get_env (emcp, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.