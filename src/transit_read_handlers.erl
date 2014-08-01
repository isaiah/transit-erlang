-module(transit_read_handlers).
-export([handler/1]).

-include_lib("transit_format.hrl").

handler(Tag) ->
  case Tag of
    ?Null ->
      fun(_) -> undefined end;
    ?Boolean ->
      fun(Rep) ->
        case Rep of
          "t" -> true;
          "f" -> false
        end
      end;
    ?Int ->
      list_to_liteger;
    ?Float ->
      list_to_float;
    _ -> % ?QUOTE
      fun(Rep) -> Rep end
  end.
