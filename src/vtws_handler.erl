-module(vtws_handler).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts, #{
        idle_timeout => 300000000}}.

websocket_init(State) ->
    {[], State}.

websocket_handle({text, Msg}, State) ->
    Data = jiffy:decode(Msg),
    {[{Type, Content}]} = Data,
    case Type of
        <<"login">> -> 
            vality_test_server:store_user(Content, self());
        <<"msg">> ->
            Login = vality_test_server:get_login_by_pid(self()),
            vality_test_server:send_message(Login, Content)
    end,
    {[], State};

websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({message, Login, Msg}, State) ->
    NewMsg = list_to_binary([Login, <<" : ">>, Msg]),
    {[{text, NewMsg}], State};
    
websocket_info(_Info, State) ->
    {[], State}.
