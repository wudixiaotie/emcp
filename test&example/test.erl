-module (test).

-compile (export_all).

% db.feeds.insert ({id: "1069601", photo: {mention: ["10801","10792","20861","20863", "1", "2"]}})
% db.likes.insert ({like_to: "1069601", author: {uid: ["10801","10792","20863"]}})
% db.comments.insert ({comment_to: "1069601", author: {uid: ["10801", "1", "2"]}})
a () ->
    emcp:start (),
    emcp:find (<<"likes">>, {like_to, <<"1069601">>}, {<<"author.uid">>, true}),
    emcp:find (<<"likes">>, {like_to, <<"1069601">>}, {<<"author.uid">>, true}),
    emcp:find_only (<<"likes">>, {like_to, <<"1069601">>}, {<<"author.uid">>, true}).


load_test () ->
    emcp:start (),
    load_test (100).

load_test (Current) ->
    case Current > 0 of
        true ->
            spawn (fun () ->
                    emcp:find_only (<<"likes">>,
                                    {like_to, <<"1069601">>},
                                    {<<"author.uid">>, true})
                end),
            load_test (Current - 1);
        false ->
            ok
    end.

b () ->
    emcp:start (),
    Pid = list_to_pid ("<0.100.0>"),
    supervisor:terminate_child (mc_w_sup, Pid).

get_random () ->
    get_random (1).

get_random (Max) ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes (12),
    random:seed (A, B, C),
    random:uniform(Max).

random_test (Current) ->
    case Current > 0 of
        true ->
            spawn (fun () ->
                    io:format ("=============~p~n", [get_random (100)])
                end),
            random_test (Current - 1);
        false ->
            ok
    end.
