-module(vality_test_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([store_user/2, remove_user/1]).
-export([send_message/2, get_login_by_pid/1, bot_init/0]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Arg) ->
  gen_server:start_link({local,?MODULE},?MODULE, Arg, []).

get_login_by_pid(Pid) ->
  gen_server:call(?SERVER, {get_login, Pid}).

store_user(Login, Pid) ->
  gen_server:cast(?SERVER, {store_user, Login, Pid}).

remove_user(Login) ->
  gen_server:cast(?SERVER, {remove_user, Login}).

send_message(Login, Message) ->
  gen_server:cast(?SERVER, {send_message, Login, Message}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  spawn_link(?SERVER, bot_init, []),
  {ok, #{users => #{}, pids => #{}}}.

%% ------------------------------------------------------------------

%% ------------------------------------------------------------------

handle_call({get_login, Pid}, _From, State) ->
    Logins = maps:get(pids, State),
    Login = maps:get(Pid, Logins),
    {reply, Login, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
%% ------------------------------------------------------------------
handle_cast({store_user, Login, Pid}, State) ->
  Users = maps:get(users, State),
  NewUsers = maps:put(Login, Pid, Users),
  Pids = maps:get(pids, State),
  NewPids = maps:put(Pid, Login, Pids),
  UpdState = maps:update(pids, NewPids, State),
  {noreply, maps:update(users, NewUsers, UpdState)};

handle_cast({remove_user, Login}, State) ->
  Users = maps:get(users, State),
  NewUsers = maps:remove(Login,Users),
  {noreply, maps:update(users, NewUsers, State)};

handle_cast({send_message, Login, Message}, State) ->
  Users = maps:to_list(maps:get(users, State)),
  [Pid ! {message, Login, Message} || {_, Pid} <- Users],
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.
%% ------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
bot_init()->
  Name = <<"Kotyara">>,
  Messages = [<<"Meow">>, <<"Meooow">>, <<"Whoof">>],
  Timer = 10000,
  bot_activate(Name,Messages, Timer).

bot_activate(Name, Messages, Timer) ->
  timer:sleep(Timer),
  NMsg = random:uniform(length(Messages)),
  Message = lists:nth(NMsg, Messages),
  send_message(Name, Message),
  bot_activate(Name, Messages, Timer).