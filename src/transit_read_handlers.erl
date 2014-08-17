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
  fun(Rep) ->
      transit_types:datetime(transit_utils:ms_to_timestamp(binary_to_integer(Rep)))
  end;

handler(?VerboseDate) ->
  fun(Rep) ->
      <<Y:8/binary-unit:4,"-",MM:8/binary-unit:2,"-",D:8/binary-unit:2,"T",H:8/binary-unit:2,":",M:8/binary-unit:2,":",S:8/binary-unit:2,".",MS:8/binary-unit:3,"Z">> = Rep,
      DateTime = lists:map(fun erlang:binary_to_integer/1, [Y,MM,D,H,M,S,MS]),
      BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
      [UTC] = calendar:local_time_to_universal_time_dst({list_to_tuple(lists:sublist(DateTime, 3)),
                                                         list_to_tuple(lists:sublist(DateTime, 3, 3))}),
      Secs = calendar:datetime_to_gregorian_seconds(UTC) - BaseDate,
      transit_types:datetime(transit_utils:ms_to_timestamp(Secs * 1000 + lists:last(DateTime)))
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
