-module (emcp).

%% APIs
-export ([start/0, stop/0, find/2, find/3, find_only/3]).


start () ->
    application:start (emcp).

stop () ->
    application:stop (emcp).

find (Collection, Selector) ->
    mc_w:find (Collection, Selector).

find (Collection, Selector, Projector) ->
    mc_w:find (Collection, Selector, Projector).

find_only (Collection, Selector, Projector) ->
    mc_w:find_only (Collection, Selector, Projector).