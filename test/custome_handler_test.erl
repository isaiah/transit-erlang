-module(custome_handler_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("transit_format.hrl").

-export([handler/1]).
-record(point, {x,y}).

custom_handler_test() ->
  P = #point{x=1.5, y=2.5},
  ?assertEqual(<<"[\"~#point\",[1.5,2.5]]">>, transit_writer:write(P, [{format,json},{handler, ?MODULE}])).

%%% custom handler callback
handler(Obj) ->
  case is_record(Obj, point) of
    true ->
      #write_handler{tag=fun(_) -> <<"point">> end,
                     rep=fun(#point{x=X, y=Y}) -> [X, Y] end};
    false ->
      undefined
  end.
