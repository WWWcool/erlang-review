-module(web_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-type state() :: #{}.

-spec init(_, _) -> {cowboy_websocket, _, _}.

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

-spec websocket_init(term()) -> {ok, state()}.

websocket_init(_) ->
    logger:alert("websocket_init ..."),
    conn_group:join(),
    {ok, #{}}.

-spec websocket_handle(_, state()) -> {reply, _, state()} | {ok, state()}.

websocket_handle({text, Text}, State) ->
    #{<<"type">> := Type, <<"data">> := Data} = jsx:decode(Text, [return_maps]),
    logger:alert("Handle message - ~p", [{Type, Data}]),
    {Reply, NewState} = case {Type, Data} of
        {<<"send_message">>, Message} ->
            Nick = maps:get(nick, State, <<"Noname">>),
            ChatLine = erlang:iolist_to_binary([Nick, <<": ">>, Message]),
            conn_group:broadcast_message(ChatLine),
            {{new_message, ChatLine}, State};
        {<<"set_nickname">>, Nick} ->
            Action = <<" joined chat.">>,
            ChatLine = erlang:iolist_to_binary([Nick, Action]),
            conn_group:broadcast_message(ChatLine),
            {{new_message, ChatLine}, maps:put(nick, Nick, State)};
        Unknown ->
            logger:alert("get unknown message - ~p", [Unknown]),
            {disconnect, State}
    end,
    {reply, {text, encode(Reply)}, NewState};
websocket_handle(_Data, State) ->
    logger:alert("get unknown message - ~p", [_Data]),
    {ok, State}.

-spec websocket_info(_, state()) -> {reply, _, state()} | {ok, state()}.

% get message
websocket_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    logger:alert("server down with reason - ~p", [Reason]),
    {reply, {text, encode({service_message, <<"Server down">>})}, State};
websocket_info({chat_line, ChatLine}, State) ->
    {reply, {text, encode({new_message, ChatLine})}, State};
websocket_info(Info, State) ->
    logger:alert("Get unexpected info - ~p", [Info]),
    {ok, State}.

encode({Type, Data}) ->
    jsx:encode(#{type => Type, data => Data}).

-spec terminate(_, _, state()) -> ok.

terminate(_Reason, _PartialReq, _State) ->
    conn_group:leave(),
    ok.

