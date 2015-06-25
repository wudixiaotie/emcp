%% ===================================================================
%% Author xiaotie
%% 2015-6-24
%% mongo client connection manager
%% ===================================================================

-module (mc_c_manager).

-behaviour (gen_server).

% APIs
-export ([start_link/0, get_conn/0, update_conn/1]).

% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {max :: integer (), index :: integer()}).

%% ===================================================================
%% APIs
%% ===================================================================

start_link () ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

get_conn () ->
    {ok, Key} = gen_server:call (?MODULE, index),
    [{_, Conn}] = ets:lookup (mongo_conns, Key),
    {ok, Key, Conn}.

update_conn (Key) ->
    [{_, Conn}] = ets:lookup (mongo_conns, Key),
    case is_process_alive (Conn) of
        true ->
            {ok, Conn};
        false ->
            {ok, NewConn} = new_connection (),
            ets:update_element (test, Key, {2, NewConn}),
            {ok, NewConn}
    end.
    

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init ([]) ->
    % write_concurrency:true 并发写 锁记录而不锁表
    ets:new (mongo_conns, [set, named_table, public, {read_concurrency, true}]),
    Max = emcp_config:max_conn (),
    F = fun (Index) ->
            {ok, Conn} = new_connection (),
            ets:insert (mongo_conns, [{Index, Conn}])
        end,
    lists:foreach (F, lists:seq (1, Max)),
    {ok, #state{max = Max, index = 1}}.


handle_call (index, _From, #state{max = Max, index = Index} = State) ->
    case Max - Index > 0 of
        true ->
            NewState = State#state{index = Index + 1};
        false ->
            NewState = State#state{index = 1}
    end,
    {reply, {ok, Index}, NewState};
handle_call (_Request, _From, State) -> {reply, nomatch, State}.


handle_cast (_Msg, State) -> {noreply, State}.
handle_info (_Info, State) -> {noreply, State}.
terminate (_Reason, _State) -> ok.
code_change (_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

new_connection () ->
    supervisor:start_child (mc_c_sup, []).