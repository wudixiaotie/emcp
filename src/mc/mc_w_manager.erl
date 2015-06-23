%% ===================================================================
%% Author xiaotie
%% 2015-6-19
%% Mongo client worker manager
%% ===================================================================

-module (mc_w_manager).

-behaviour (gen_server).

% APIs
-export ([start_link/1, get_worker/0, worker_free/1, free_worker_count/0]).

% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

% size = pool size
% fkq = free mongo client worker process pid queue
-record (state, {size :: integer(), fkq :: queue:queue()}).


%% ===================================================================
%% Apis
%% ===================================================================

start_link (Size) ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [Size], []).

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

init ([Size]) ->
    {ok, FKQ} = loop_start_workers (Size, queue:new ()),
    State = #state{size = Size, fkq = FKQ},
    {ok, State}.


handle_call (get_worker, _From, #state{size = Size, fkq = FKQ} = State) ->
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
            % {ok, Pid} = spawn_mc_w (),
            % {reply, Pid, State}
            % 进程不够不增加进程
            % 检查现有的worker数量是否小于pool size，如果小于则增加
            {count, TotalWCount} = total_worker_count (),
            case TotalWCount < Size of
                true -> {reply, spawn_mc_w (), State};
                false -> {reply, {error, no_free_worker}, State}
            end
    end;
handle_call (queue_len, _From, #state{fkq = FKQ} = State) ->
    {reply, queue:len (FKQ), State};
handle_call (_Request, _From, State) -> {reply, nomatch, State}.


handle_cast ({worker_free, WorkerPid}, #state{fkq = FKQ} = State) ->
    NewFKQ = queue:in (WorkerPid, FKQ),
    {noreply, State#state{fkq = NewFKQ}};
handle_cast (_Msg, State) -> {noreply, State}.


handle_info (_Info, State) -> {noreply, State}.
terminate (_Reason, _State) -> ok.
code_change (_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

loop_start_workers (Current, FKQ) ->
    case Current > 0 of
        false -> {ok, FKQ};
        true ->
            Next = Current - 1,
            {ok, WorkerPid} = spawn_mc_w (),
            NewFKQ = queue:in (WorkerPid, FKQ),
            loop_start_workers (Next, NewFKQ)
    end.

spawn_mc_w () ->
    supervisor:start_child (mc_w_sup, []).

total_worker_count () ->
    SupPid = whereis (mc_w_sup),
    [_,_,_,{_,Count}] = supervisor:count_children (SupPid),
    {count, Count}.