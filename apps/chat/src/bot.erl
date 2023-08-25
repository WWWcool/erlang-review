-module(bot).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  code_change/3]).

-define(SERVER, ?MODULE).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  timer:send_after(1000, wake_up),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(wake_up, State) ->
  conn_group:broadcast_message(<<"~Bot sweeps up the floor.~">>),
  timer:send_after(rand:uniform(10_000) + 1000, wake_up),
  {noreply, State};
handle_info(Info, State) ->
  logger:alert("Bot got unexpected info - ~p", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

