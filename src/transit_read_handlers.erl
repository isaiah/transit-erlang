-module(transit_read_handlers).
-export([handler/1]).

-include_lib("transit_format.hrl").

handler(?Null) ->
  fun(_) -> undefined end;
handler(?Boolean) ->
  fun(Rep) ->
    case Rep of
      "t" -> true;
      "f" -> false
    end
  end;
handler(?Int) ->
  list_to_liteger;
handler(?Float) ->
  list_to_float;
handler(?QUOTE) ->
  fun(Rep) -> Rep end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handler_test() ->
  F = handler(<<"'">>),
  ok = F(ok).
-endif.
