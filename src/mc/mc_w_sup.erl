%% ===================================================================
%% Author xiaotie
%% 2015-6-17
%% mongo client worker supervisor
%% ===================================================================

-module (mc_w_sup).

-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% Supervisor callbacks
-export ([init/1]).

%% Helper macro for declaring children of supervisor
-define (CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, temporary, brutal_kill, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init ([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD(mc_w, [])] } }.