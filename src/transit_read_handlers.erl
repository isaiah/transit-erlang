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
  fun(Rep) ->
      case Rep of
        null ->
          undefined;
        _ ->
          Rep
      end
  end;
handler(?Set) ->
  fun(Rep) ->
      sets:from_list(Rep)
  end;
handler(?Keyword) ->
  fun(Rep) ->
      binary_to_atom(Rep, utf8)
  end;
handler(_) ->
  fun(Rep) ->
      Rep
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handler_test() ->
  F = handler(<<"'">>),
  ok = F(ok).
-endif.
