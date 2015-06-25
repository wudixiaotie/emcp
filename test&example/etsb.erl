%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%-------------------------------------------------------------------
%%% @author Rickard Green
%%% @doc
%%%   An ETS benchmark.
%%%
%%%   Probably not the most beautiful Erlang code you've seen. It was
%%%   written in a bit of a hurry.
%%%
%%% @end
%%% Created : 14 Oct 2010 by Rickard Green
%%%-------------------------------------------------------------------

-module(etsb).

-define(NO_TEST_RUNS, 5).
-define(TSIZE, 500).

-export([go/1]).

write_elog(Res, Name, XOpts, ReadUntil, Count, Loops, SC) ->
    S = erlang:system_info(schedulers),
    FileName = Name
    ++ "-" ++ opts_str(XOpts)
    ++ "-" ++ writers(ReadUntil, Count, Loops)
    ++ case SC of
           false -> "";
           _ ->
           "-sw" ++ writers(ReadUntil, SC, Loops)
       end,
    io:format("~s ~p ~p~n", [FileName, S, Res]),
    {ok, IOD} = file:open(FileName++".elog", [write,append]),
    io:format(IOD, "{~p, ~p}.~n", [S, Res]),
    file:close(IOD).

write_avg_log(Results, Name, XOpts, ReadUntil, Count, Loops, SC) ->
    S = erlang:system_info(schedulers),
    Res = lists:sum(Results)/length(Results),
    FileName = Name
    ++ "-" ++ opts_str(XOpts)
    ++ "-" ++ writers(ReadUntil, Count, Loops)
    ++ case SC of
           false -> "";
           _ ->
           "-sw" ++ writers(ReadUntil, SC, Loops)
       end,
    io:format("~s-avg ~p ~p~n", [FileName, S, Res]),
    {ok, IOD} = file:open(FileName, [write,append]),
    io:format(IOD, "~p ~p~n", [S, Res]),
    file:close(IOD).


opts_str([]) ->
    "def";
opts_str([{write_concurrency, true}]) ->
    "wc";
opts_str([{read_concurrency, true}]) ->
    "rc";
opts_str([{read_concurrency, true},{write_concurrency, true}]) ->
    "wrc";
opts_str([{write_concurrency, true},{read_concurrency, true}]) ->
    "wrc".

writers(true, X, X) ->
    "0";
writers(true, Count, _) ->
    integer_to_list(round(1/Count*100));
writers(false, X, X) ->
    "100";
writers(false, Count, _) ->
    integer_to_list(100 - round(1/Count*100)).

put_data() ->
    put(?MODULE, {"here are some", data, "to store", make_ref()}).

get_data() ->
    get(?MODULE).

get_ets_opts([]) ->
    [];
get_ets_opts([$- | Rest]) ->
    get_ets_opts(Rest);
get_ets_opts([$d, $e, $f | Rest]) ->
    [[] | get_ets_opts(Rest)];
get_ets_opts([$r, $c | Rest]) ->
    [[{read_concurrency, true}] | get_ets_opts(Rest)];
get_ets_opts([$w, $c | Rest]) ->
    [[{write_concurrency, true}] | get_ets_opts(Rest)];
get_ets_opts([$w, $r, $c | Rest]) ->
    [[{read_concurrency, true},{write_concurrency, true}]
     | get_ets_opts(Rest)];
get_ets_opts([$r, $w, $c | Rest]) ->
    [[{read_concurrency, true},{write_concurrency, true}]
     | get_ets_opts(Rest)].


go([Name, LoopsStr, ReadOnlyStr, EtsOpts]) ->
    Loops = list_to_integer(LoopsStr),
    ReadOnly = list_to_atom(ReadOnlyStr),
    Procs = 1000,
    lists:foreach(fun (XOpts) ->
              run(Name, XOpts, true, Loops, Loops, Procs, false),
              case ReadOnly of
                  true ->
                  ok;
                  _ ->
                  run(Name, XOpts, true, Loops, Loops, Procs, 100),
                  run(Name, XOpts, true, 100, Loops, Procs, false),
                  run(Name, XOpts, true, 2, Loops, Procs, false),
                  run(Name, XOpts, false, Loops, Loops, Procs, false),
                  ok
              end
          end,
          get_ets_opts(EtsOpts)),
    ok.

init(T, N) when N < ?TSIZE ->
    ets:insert(T, {N, N, N}),
    init(T, N+1);
init(_T, _N) ->
    ok.


ops(_T, _UW, _N, _C, _SC, 0) ->
    ok;
ops(T, UW, N, C, SC, Tot) when N >= ?TSIZE ->
    ops(T, UW, 0, C, SC, Tot);
ops(T, UW, N, 0, SC, Tot) ->
    case UW of
    true ->
        true = ets:insert(T, {N, Tot, get_data()});
    false ->
        [{N, _, _}] = ets:lookup(T, N)
    end,
    ops(T, UW, N+1, SC, SC, Tot-1);
ops(T, UW, N, C, SC, Tot) ->
    case UW of
    false ->
        true = ets:insert(T, {N, Tot, get_data()});
    true ->
        [{N, _, _}] = ets:lookup(T, N)
    end,
    ops(T, UW, N+1, C-1, SC, Tot-1).

run(Name, XOpts, UW, C, N, NP, SC) ->
    receive after 100 -> ok end,
    {TP, TM} = spawn_monitor(fun () ->
                     RL = repeat_list(fun () ->
                                  Caller = self(),
                                  T = fun () ->
                                      Parent = self(),
                                      put_data(),
                                      T=ets:new(x, [public | XOpts]),
                                      init(T, 0),
                                      Ps0 = repeat_list(fun () ->
                                                    spawn_link(fun () ->
                                                               put_data(),
                                                               receive go -> ok end,
                                                               ops(T, UW, N, C, C, N),
                                                               Parent ! {done, self()},
                                                               receive after infinity -> ok end
                                                           end)
                                                end,
                                                NP - case SC of
                                                     false -> 0;
                                                     _ -> 1
                                                 end),
                                      Ps = case SC of
                                          false -> Ps0;
                                          _ -> [spawn_link(fun () ->
                                                       put_data(),
                                                       receive go -> ok end,
                                                       ops(T, UW, N, SC, SC, N),
                                                       Parent ! {done, self()},
                                                       receive after infinity -> ok end
                                                   end) | Ps0]
                                           end,
                                      Start = now(),
                                      lists:foreach(fun (P) -> P ! go end, Ps),
                                      lists:foreach(fun (P) -> receive {done, P} -> ok end end, Ps),
                                      Stop = now(),
                                      lists:foreach(fun (P) ->
                                                unlink(P),
                                                exit(P, bang),
                                                M = erlang:monitor(process, P),
                                                receive
                                                    {'DOWN', M, process, P, _} -> ok
                                                end
                                            end, Ps),
                                      Res = timer:now_diff(Stop, Start)/1000000,
                                      write_elog(Res, Name, XOpts, UW, C, N, SC),
                                      Caller ! {?MODULE, self(), Res}
                                  end,
                                  TP = spawn_link(T),
                                  receive
                                  {?MODULE, TP, Res} ->
                                      Res
                                  end
                              end,
                              ?NO_TEST_RUNS),
                     write_avg_log(RL, Name, XOpts, UW, C, N, SC)
                 end),
    receive
    {'DOWN', TM, process, TP, _} -> ok
    end.

repeat_list(Fun, N) ->
    repeat_list(Fun, N, []).

repeat_list(_Fun, 0, Acc) ->
    Acc;
repeat_list(Fun, N, Acc) ->
    repeat_list(Fun, N-1, [Fun()|Acc]).