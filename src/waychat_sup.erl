-module(waychat_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Timeout, Params), {I, {I, start_link, Params}, permanent, Timeout, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
    io:format("waychat_sup start_link. Listen socket : ~p", [LSock]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSock]) ->
    io:format("waychat_sup init."),
    GatewaySup = ?CHILD(waychat_gateway_sup, supervisor, infinity, [LSock]),
    RoomSup  = ?CHILD(waychat_room_sup, supervisor, infinity, []),
    RoomRegisterServer = ?CHILD(waychat_room_register, worker, 5000, []),
    UserServer = ?CHILD(waychat_user, worker, 5000, []),
    {ok, { {one_for_one, 5, 10}, [GatewaySup, RoomSup, RoomRegisterServer, UserServer]} }.

