%% ===================================================================
%% Author xiaotie
%% 2015-6-24
%% mongo client connection manager
%% ===================================================================

-module (mc_c_manager).

-behaviour (gen_server).

% APIs
-export ([start_link/0, get_conn/0, update_conn/0]).

% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {max_conn :: integer ()}).

%% ===================================================================
%% APIs
%% ===================================================================

start_link () ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

get_conn () ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes (12),
    random:seed (A, B, C),
    {ok, MaxConn} = gen_server:call (?MODULE, max_conn),
    Random = random:uniform (MaxConn),
    [{_, Conn}] = ets:lookup (mongo_conns, Random),
    {ok, Conn}.

update_conn () ->ok.
    

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init ([]) ->
    ets:new (mongo_conns, [set, named_table, public, {write_concurrency, true}]),
    MaxConn = emcp_config:max_conn (),
    loop (MaxConn),
    {ok, #state{max_conn = MaxConn}}.


handle_call (max_conn, _From, State) -> {reply, {ok, State#state.max_conn}, State};
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

loop (Current) ->
    case Current > 0 of
        true ->
            Next = Current - 1,
            {ok, Conn} = new_connection (),
            ets:insert (mongo_conns, [{Current, Conn}]),
            loop (Next);
        false ->
            ok
    end.