%% ===================================================================
%% Author xiaotie
%% 2015-6-19
%% Mongo client worker manager
%% ===================================================================

-module (mc_w_manager).

-behaviour (gen_server).

% APIs
-export ([start_link/0, get_worker/0, worker_free/1, free_worker_count/0]).

% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

% size = pool size
% fkq = free mongo client worker process pid queue
-record (state, {size :: integer (), fkq :: queue:queue ()}).

% 启动回收的间隔时间
-define (RECYCLE_INTERVAL, 30000).


%% ===================================================================
%% APIs
%% ===================================================================

start_link () ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

-spec get_worker () -> {ok, pid()}.
get_worker () ->
    gen_server:call (?MODULE, get_worker).

worker_free (WorkerPid) ->
    gen_server:cast (?MODULE, {worker_free, WorkerPid}).

free_worker_count () ->
    QueueLen = gen_server:call (?MODULE, queue_len),
    {count, QueueLen}.


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init ([]) ->
    Size = emcp_config:pool_size (),
    {ok, FKQ} = loop_start_workers (Size, queue:new ()),
    State = #state{size = Size, fkq = FKQ},
    ready_4_recycle (),
    {ok, State}.


handle_call (get_worker, _From, #state{fkq = FKQ} = State) ->
    case queue:is_empty (FKQ) of
        false ->
            {{value, PidFromFKQ}, NewFKQ} = queue:out (FKQ),
            % 进程死掉则新建worker
            case is_process_alive (PidFromFKQ) of
                true ->
                    Pid = PidFromFKQ;
                false ->
                    {ok, Pid} = spawn_mc_w ()
            end,
            {reply, {ok, Pid}, State#state{fkq = NewFKQ}};
        true ->
            % 进程不够增加进程
            lager:info ("spawn a new worker"),
            {reply, spawn_mc_w (), State}
    end;
handle_call (queue_len, _From, #state{fkq = FKQ} = State) ->
    {reply, queue:len (FKQ), State};
handle_call (_Request, _From, State) -> {reply, nomatch, State}.


handle_cast ({worker_free, WorkerPid}, #state{fkq = FKQ} = State) ->
    NewFKQ = queue:in (WorkerPid, FKQ),
    {noreply, State#state{fkq = NewFKQ}};
handle_cast (_Msg, State) -> {noreply, State}.


handle_info (recycle, #state{size = Size, fkq = FKQ} = State) ->
    {count, TotalWCount} = total_worker_count (),
    % 现有worker数量 - 规定的worker数量 = 超出的worker数量
    RedundantCount = TotalWCount - Size,
    if
        % 超出的worker数量大于0，则回收worker
        RedundantCount > 0 ->
            QueueLength = queue:len (FKQ),
            QueueReserve = Size div 10,
            % 队列长度 - 需要free worker queue队列中至少储备的worker数量 = 可以回收的worker数量
            QueueCanRecycleCount = QueueLength - QueueReserve,


            case QueueCanRecycleCount > 0 of
                % 队列中有可以回收的worker
                true ->
                    case QueueCanRecycleCount >= RedundantCount of
                        % 可以回收的worker数量 >= 超出的worker数量
                        true ->
                            RecycleCount = RedundantCount;
                        % 可以回收的worker数量 < 超出的worker数量
                        % 说明原有Size的worker不一定能完全满足现有需求，不能全部释放超出的worker
                        false ->
                            RecycleCount = QueueCanRecycleCount div 2 + 1
                    end,
                    lager:info("mc_w_manager recycle: kill ~p workers.", [RecycleCount]),
                    {ok, NewFKQ} = loop_kill_workers (RecycleCount, FKQ),
                    NewState = State#state{fkq = NewFKQ};
                % 空闲的worker少于5则不回收
                false ->
                    lager:info("mc_w_manager recycle: kill 0 worker."),
                    NewState = State
            end;
        RedundantCount == 0 ->
            lager:info("mc_w_manager recycle: no need."),
            NewState = State;
        % 超出的worker数量小于0，则增加worker
        RedundantCount < 0 ->
            AbsRedundantCount = erlang:abs (RedundantCount),
            lager:info("mc_w_manager recycle: add ~p workers.", [AbsRedundantCount]),
            {ok, NewFKQ} = loop_start_workers (AbsRedundantCount, FKQ),
            NewState = State#state{fkq = NewFKQ}
    end,
    ready_4_recycle (),
    {noreply, NewState};
handle_info (_Info, State) -> {noreply, State}.
terminate (_Reason, _State) -> ok.
code_change (_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

loop_start_workers (Index, FKQ) ->
    case Index > 0 of
        false -> {ok, FKQ};
        true ->
            Next = Index - 1,
            {ok, WorkerPid} = spawn_mc_w (),
            NewFKQ = queue:in (WorkerPid, FKQ),
            loop_start_workers (Next, NewFKQ)
    end.

loop_kill_workers (Index, FKQ) ->
    case Index > 0 of
        false -> {ok, FKQ};
        true ->
            Next = Index - 1,
            {{value, WorkerPid}, NewFKQ} = queue:out (FKQ),
            terminate_mc_w (WorkerPid),
            loop_kill_workers (Next, NewFKQ)
    end.

spawn_mc_w () ->
    supervisor:start_child (mc_w_sup, []).

terminate_mc_w (WorkerPid) ->
    supervisor:terminate_child (mc_w_sup, WorkerPid).

total_worker_count () ->
    SupPid = whereis (mc_w_sup),
    [_,_,_,{_,Count}] = supervisor:count_children (SupPid),
    {count, Count}.

ready_4_recycle () ->
    erlang:send_after (?RECYCLE_INTERVAL, self (), recycle).