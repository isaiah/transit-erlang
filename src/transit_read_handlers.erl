-module(transit_read_handlers).
-export([handler/1]).

-include_lib("transit_format.hrl").

handler(?CMap) ->
  fun(Rep) ->
      transit_utils:map_rep(list_to_proplist(Rep))
  end;
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
  fun binary_to_integer/1;
handler(?Float) ->
  fun binary_to_float/1;
handler(?BigInt) ->
  fun binary_to_integer/1;
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
handler(?List) ->
  fun(Rep) ->
      transit_types:list(Rep)
  end;
handler(?Keyword) ->
  fun(Rep) ->
      binary_to_atom(Rep, utf8)
  end;
handler(?Symbol) ->
  fun(Rep) ->
      transit_types:symbol(Rep)
  end;
handler(?Date) ->
  fun(Rep) when is_integer(Rep) ->
      transit_types:datetime(transit_utils:ms_to_timestamp(Rep));
     (Rep) ->
      transit_types:datetime(transit_utils:ms_to_timestamp(binary_to_integer(Rep)))
  end;

handler(?VerboseDate) ->
  fun(Rep) ->
      transit_types:datetime(transit_utils:iso_8601_to_timestamp(Rep))
  end;
handler(?UUID) ->
  fun(U=[_, _]) ->
      transit_types:uuid(list_to_binary(transit_utils:uuid_to_string(U)));
     (U) ->
      transit_types:uuid(U)
  end;
handler(_) ->
  undefined.

list_to_proplist([]) -> [];
list_to_proplist([K,V|Tail]) -> [{K,V}|list_to_proplist(Tail)].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handler_test() ->
  F = handler(<<"'">>),
  ok = F(ok).
-endif.
