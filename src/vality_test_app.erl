%%%-------------------------------------------------------------------
%% @doc vality_test public API
%% @end
%%%-------------------------------------------------------------------

-module(vality_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, vality_test, "index2.html"}},
			{"/websocket", vtws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, vality_test, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	vality_test_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).

%%====================================================================
%% Internal functions
%%====================================================================
