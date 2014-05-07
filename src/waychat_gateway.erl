-module(waychat_gateway).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
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
    io:format("gateway start_link, Listen socket:~p", [LSock]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [LSock], []).

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
    io:format("gateway init, Listen socket:~p", [LSock]),
    inet:setopts(LSock, [{active, once}, {packet, 2}, binary]),
    {ok, #state{lsock = LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, State) ->
    io:format("gateway handle_info, data:~p", [Data]),
    inet:setopts(Sock, [{active, once}]),
    ok = gen_tcp:send(Sock, <<"Echo back : ", Data/binary>>),
    {noreply, State};

handle_info({tcp_closed, Sock}, State) ->
    io:format("gateway handle_info, tcp_closed"),
    {stop, normal, State};

handle_info(timeout, #state{lsock = LSock} = State) ->
    io:format("gateway handle_info, timeout"),
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, {IP, _Port}} = inet:peername(Sock),
    waychat_gateway_sup:start_child(),
    {noreply, State#state{sock=Sock, ip=IP}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=Sock} = State) ->
    (catch gen_tcp:close(Sock)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

