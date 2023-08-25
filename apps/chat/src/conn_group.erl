-module(conn_group).

-export([join/0, leave/0, broadcast_message/1]).

-define(PG_GROUP, connections).


join() ->
  pg:join(?PG_GROUP, self()).

leave() ->
  pg:leave(?PG_GROUP, self()).


broadcast_message(ChatLine) ->
  broadcast_message(ChatLine, pg:get_members(?PG_GROUP)).

broadcast_message(ChatLine, [Conn | Rest]) when Conn == self() ->
  broadcast_message(ChatLine, Rest);
broadcast_message(ChatLine, [Conn | Rest]) ->
  logger:alert("sending chat line to ~p", [Conn]),
  Conn ! {chat_line, ChatLine},
  broadcast_message(ChatLine, Rest);
broadcast_message(_ChatLine, []) ->
  ok.
