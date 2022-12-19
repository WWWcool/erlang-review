%%%-------------------------------------------------------------------
%% @doc vality_test top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vality_test_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [
    #{id => vality_test_server, start => {vality_test_server, start_link, [[]]}, type => worker}
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
