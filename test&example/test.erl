-module (test).

-compile (export_all).

% db.jvm_feeds.insert ({id: "1069601", photo: {mention: ["10801","10792","20861","20863", "1", "2"]}})
% db.jvm_likes.insert ({like_to: "1069601", author: {uid: ["10801","10792","20863"]}})
% db.jvm_comments.insert ({comment_to: "1069601", author: {uid: ["10801", "1", "2"]}})
a () ->
    emcp:start (),
    emcp:find (<<"jvm_likes">>, {like_to, <<"1069601">>}, {<<"author.uid">>, true}),
    emcp:find (<<"jvm_likes">>, {like_to, <<"1069601">>}, {<<"author.uid">>, true}),
    emcp:find_only (<<"jvm_likes">>, {like_to, <<"1069601">>}, {<<"author.uid">>, true}).


load_test () ->
    emcp:start (),
    F = fun (_Index) ->
            spawn (test, loop, [1])
        end,
    lists:foreach (F, lists:seq (1, 100)).

loop (Index) ->
    case Index == 4 of
        true -> ok;
        false ->
            emcp:find_only (<<"jvm_likes">>,
                            {like_to, <<"1069601">>},
                            {<<"author.uid">>, true}),
            receive
            after
                1000 ->
                    loop (Index + 1)
            end
    end.

tc () ->
    tc:tc (emcp, find_only, [<<"jvm_likes">>, {like_to, <<"1069601">>}, {<<"author.uid">>, true}]).

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
