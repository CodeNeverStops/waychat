-module(waychat_app).
-author('lokiwuxi@gmail.com').

-behaviour(application).

-define(PORT, 9999).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("waychat_app start. start_type:~p,start_args:~p", [_StartType, _StartArgs]),
    Opts = [binary, {packet, 2}, {reuseaddr, true},
        {keepalive, true}, {backlog, 30}, {active, false}],
    ListenPort = get_app_env(listen_port, ?PORT),
    {ok, LSock} = gen_tcp:listen(ListenPort, Opts),
    case waychat_sup:start_link(LSock) of
        {ok, Pid} ->
            waychat_gateway_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of 
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error       -> Default
            end
    end.
