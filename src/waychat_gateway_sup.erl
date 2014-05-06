-module(waychat_gateway_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSock]) ->
    GatewayServer = {waychat_gateway, {waychat_gateway, start_link, [LSock]}, permanent, 5000, worker, [waychat_gateway]},
    Children = [GatewayServer],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

