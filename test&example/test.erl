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