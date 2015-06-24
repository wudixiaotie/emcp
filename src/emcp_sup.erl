-module (emcp_sup).

-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% Supervisor callbacks
-export ([init/1]).

%% Helper macro for declaring children of supervisor
-define (CHILD (I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link () ->
    supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init ([]) ->
    {ok, {
          {one_for_one, 5, 10},
          [
            ?CHILD (mc_c_sup, [], supervisor),
            ?CHILD (mc_c_manager, [], worker),
            ?CHILD (mc_w_sup, [], supervisor),
            ?CHILD (mc_w_manager, [], worker)
          ]
         } }.