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

init(_Args) ->
    init_store(),
    {ok, dict:new()}.

handle_call({user_register, Nickname, Password}, _From, State) ->
    PasswordMD5 = server_util:md5(Password),
    user_save(Nickname, PasswordMD5),
    {reply, ok, State};

handle_call({user_login, Nickname, Password}, _From, State) ->
    User = user_get(Nickname),
    PasswordMD5 = server_util:md5(Password),
    if 
        User#chat_user.password =:= PasswordMD5 ->
            % generate session id
            {_, Secs, _} = erlang:now(),
            SessionId = server_util:md5(list_to_binary([Nickname|[Secs]])),
            NewState = dict:store(SessionId, Nickname, State),
            {reply, {ok, SessionId}, NewState};
        true ->
            {reply, {error, user_not_found}, State}
    end;

handle_call({user_logout, SessionId}, _From, State) ->
    case dict:find(SessionId, State) of
        {ok, _} ->
            NewState = dict:erase(SessionId, State),
            {reply, ok, NewState};
        error ->
            {reply, {error, user_not_online}, State}
    end;

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
        qlc:e(Query) end,
    {atomic, Users} = mnesia:transaction(F),
    Users.
