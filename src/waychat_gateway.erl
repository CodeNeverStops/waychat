-module(waychat_gateway).
-behaviour(gen_server).
-record(state, {lsock, sock, ip}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(LSock) ->
    io:format("gateway start_link, Listen socket:~p~n", [LSock]),
    % 由于这里是simple_one_for_one，所以不能用{local, ServerName}来注册，
    % 多个同名进程会有冲突，导致start_link失败
    gen_server:start_link(?MODULE, [LSock], []).

user_register() ->
    ok.

user_login() ->
    ok.

user_logout() ->
    ok.

send_message() ->
    ok.

create_room() ->
    ok.


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([LSock]) ->
    inet:setopts(LSock, [{active, once}, {packet, 2}, binary]),
    io:format("gateway init~n"),
    {ok, #state{lsock = LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, State) ->
    inet:setopts(Sock, [{active, once}]),
    io:format("gateway send back data:~p~n", [Data]),
    ok = gen_tcp:send(Sock, <<"Echo back : ", Data/binary>>),
    {noreply, State};

handle_info({tcp_closed, _Sock}, State) ->
    io:format("gateway tcp_closed~n"),
    {stop, normal, State};

handle_info(timeout, #state{lsock = LSock} = State) ->
    io:format("gateway accept start~n"),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("gateway accept end~n"),
    waychat_gateway_sup:start_child(),
    {ok, {IP, _Port}} = inet:peername(Sock),
    {noreply, State#state{sock=Sock, ip=IP}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=Sock} = _State) ->
    (catch gen_tcp:close(Sock)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

