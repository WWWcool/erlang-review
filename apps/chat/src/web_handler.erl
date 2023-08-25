-module(web_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-type state() :: #{}.

-define(PG_GROUP, connections).

-spec init(_, _) -> {cowboy_websocket, _, _}.

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

-spec websocket_init(term()) -> {ok, state()}.

websocket_init(_) ->
    logger:alert("websocket_init ..."),
    pg:join(?PG_GROUP, self()),
    {ok, #{}}.

-spec websocket_handle(_, state()) -> {reply, _, state()} | {ok, state()}.

websocket_handle({text, Text}, State) ->
    #{<<"type">> := Type, <<"data">> := Data} = jsx:decode(Text, [return_maps]),
    logger:alert("Handle message - ~p", [{Type, Data}]),
    {Reply, NewState} = case {Type, Data} of
        {<<"send_message">>, Message} ->
            Nick = maps:get(nick, State, <<"Noname">>),
            ChatLine = erlang:iolist_to_binary([Nick, <<": ">>, Message]),
            broadcast_message(ChatLine),
            {{new_message, ChatLine}, State};
        {<<"set_nickname">>, Nick} ->
            Action = <<" joined chat.">>,
            ChatLine = erlang:iolist_to_binary([Nick, Action]),
            broadcast_message(ChatLine),
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
    pg:leave(?PG_GROUP, self()),
    ok.

broadcast_message(ChatLine) ->
    broadcast_message(ChatLine, pg:get_members(?PG_GROUP)).

broadcast_message(ChatLine, [Conn | Rest]) when Conn == self() ->
    broadcast_message(ChatLine, Rest);
broadcast_message(ChatLine, [Conn | Rest]) ->
    logger:alert("sending chat line to ~p", [Conn]),
    Conn ! {chat_line, ChatLine},
    broadcast_message(ChatLine, Rest);
broadcast_message(ChatLine, []) ->
  ok.
