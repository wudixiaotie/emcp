%% ===================================================================
%% Author xiaotie
%% 2015-6-17
%% mongo client worker
%% ===================================================================

-module (mc_w).

-behaviour (gen_server).

% APIs
-export ([start_link/0, find/2, find/3, find_only/3]).

% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {conn :: pid()}).


%% ===================================================================
%% Apis
%% ===================================================================

start_link () ->
    gen_server:start_link (?MODULE, [], []).

find (Collection, Selector) ->
    call ({find, Collection, Selector}).

find (Collection, Selector, Projector) ->
    call ({find, Collection, Selector, Projector}).

find_only (Collection, Selector, Projector) ->
    call ({find_only, Collection, Selector, Projector}).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init ([]) ->
    case new_connection () of
        {ok, Connection} ->
            {ok, #state{conn = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.


handle_call ({find, Collection, Selector}, _From, State) ->
    {NewConnection, MongoResult} = get_data (State#state.conn, Collection, Selector),
    Reply = {ok, MongoResult},
    {reply, Reply, State#state{conn = NewConnection}};
handle_call ({find, Collection, Selector, Projector}, _From, State) ->
    {NewConnection, MongoResult} = get_data (State#state.conn, Collection, Selector, Projector),
    Reply = {ok, MongoResult},
    {reply, Reply, State#state{conn = NewConnection}};
handle_call ({find_only, Collection, Selector, Projector}, _From, State) ->
    {NewConnection, MongoResult} = get_data (State#state.conn, Collection, Selector, Projector),
    Reply = {ok, loop_result (MongoResult)},
    {reply, Reply, State#state{conn = NewConnection}};

%% nomatch
handle_call (_Request, _From, State) -> {reply, nomatch, State}.



handle_cast (_Msg, State) -> {noreply, State}.
handle_info (_Info, State) -> {noreply, State}.
terminate (_Reason, _State) -> ok.
code_change (_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

new_connection () ->
    {Host, Port, DB} = emcp_config:hpd (),
    mongo:connect(DB, [{host, Host}, {port, Port}]).


call (Request) ->
    TimeOut = emcp_config:timeout (),
    case mc_w_manager:get_worker () of
        {ok, WorkerPid} ->
            Reply = (catch gen_server:call (WorkerPid, Request, TimeOut)),
            % 让worker处于空闲状态
            mc_w_manager:worker_free (WorkerPid),
            case Reply of
                {ok, Result} ->
                    {ok, Result};
                {'EXIT', {timeout, _}} ->
                    {error, timeout};
                {'EXIT', Error} ->
                    {error, Error};
                Any ->
                    {error, Any}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


get_data (Connection, Collection, Selector) ->
    get_data (Connection, Collection, Selector, {}).


get_data (Connection, Collection, Selector, Projector) ->
    Reply = (catch mongo:find (Connection, Collection, Selector, Projector)),
    case is_pid (Reply) of
        true ->
            Cursor = Reply,
            NewConnection = Connection;
        false ->
            {ok, NewConnection} = new_connection (),
            Cursor = mongo:find (NewConnection, Collection, Selector, Projector)
    end,
    Result = mc_cursor:rest (Cursor),
    % 创建Cursor的进程如果死掉，则所创建的Cursor自动销毁
    mc_cursor:close (Cursor),
    {NewConnection, Result}.


%% ===================================================================
%% Business Internal functions
%% ===================================================================

merge_remove_duplicates (ListOfLists) ->
    lists:usort (lists:flatten (ListOfLists)).

loop_result (MongoResult) -> loop_result (MongoResult, []).
loop_result ([], ResultList) -> merge_remove_duplicates (ResultList);
loop_result ([{_, _, _, {}}|T], ResultList) ->
    loop_result (T, [ResultList]);
loop_result ([{_, _, _, {_, List}}|T], ResultList) ->
    loop_result (T, [List|ResultList]).