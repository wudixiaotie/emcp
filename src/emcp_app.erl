-module (emcp_app).

-behaviour (application).

%% Application callbacks
-export ([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start (_StartType, _StartArgs) ->
    application:start (bson),
    application:start (crypto),
    application:start (mongodb),
    emcp_sup:start_link ().

stop (_State) ->
    application:stop (mongodb),
    application:stop (crypto),
    application:stop (bson),
    ok.
