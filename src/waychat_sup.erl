-module(waychat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Timeout), {I, {I, start_link, []}, permanent, Timeout, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    GatewaySup = ?CHILD(waychat_gateway_sup, supervisor, infinity),
    RoomSup  = ?CHILD(waychat_room_sup, supervisor, infinity),
    RoomRegisterServer = ?CHILD(waychat_room_register, worker, 5000),
    UserServer = ?CHILD(waychat_user, worker, 5000),
    {ok, { {one_for_one, 5, 10}, [GatewaySup, RoomSup, RoomRegisterServer, UserServer]} }.

