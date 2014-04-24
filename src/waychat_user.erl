-module(waychat_user).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("stdlib/include/qlc.hrl").

-record(chat_user, {nickname, password, created_on}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Nickname, Password) ->
    gen_server:call({local, ?SERVER}, {user_register, Nickname, Password}).

login(Nickname, Password) ->
    gen_server:call({local, ?SERVER}, {user_login, Nickname, Password}).

logout(SessionId) ->
    gen_server:call({local, ?SERVER}, {user_logout, SessionId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    init_store(),
    {ok, Args}.

handle_call({user_register, Nickname, Password}, _From, State) ->
    user_save(Nickname, Password),
    {reply, ok, State};

handle_call({user_login, Nickname, Password}, _From, State) ->
    User = user_get(Nickname),
    {reply, ok, State};

handle_call({user_logout, SessionId}, _From, State) ->
    {reply, ok, State};

handle_call(stop, _From, State) ->
    mnesia:stop(),
    {stop, normal, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

init_store() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    try
        mnesia:table_info(chat_user, type)
    catch
        exit: _ ->
            mnesia:create_table(chat_user, [{attributes, record_info(fields, chat_user)}, {type, bag}, {disc_copies, [node()]}])
    end.

user_save(Nickname, Password) ->
    F = fun() ->
        {_, CreatedOn, _} = erlang:now(),
        mnesia:write(#chat_user{nickname=Nickname, password=Password, created_on=CreatedOn}) end,
    mnesia:transaction(F).

user_get(Nickname) ->
    F = fun() ->
        Query = qlc:q([M || M <- mnesia:table(chat_user), M#chat_user.nickname =:= Nickname]),
        Results = qlc:e(Query) end,
    {atomic, Users} = mnesia:transaction(F),
    Users.

user_session(SessionId) ->
    ok.
