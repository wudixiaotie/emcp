%% ===================================================================
%% Author xiaotie
%% 2015-6-24
%% mongo client connection supervisor
%% ===================================================================

-module (mc_c_sup).

-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% Supervisor callbacks
-export ([init/1]).

%% Helper macro for declaring children of supervisor
-define (CHILD(Mod, Args), {Mod, {Mod, connect, Args}, temporary, brutal_kill, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init ([]) ->
    {Host, Port, DB} = emcp_config:hpd (),
    Args = [DB, [{host, Host}, {port, Port}]],
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD(mongo, Args)] } }.