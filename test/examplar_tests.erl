-module(examplar_tests).
-include_lib("eunit/include/eunit.hrl").

-define(Examplar, "../../transit-format/examples/0.8/simple/").

examplar_test_() ->
  transit:start(),
  examplar("nil", undefined).

examplar(Name, Val) ->
  [fun() ->
       {ok, Data} = file:read_file(?Examplar ++ Name ++ "." ++ Ext),
       L = bit_size(Data) - 8,
       <<D:L/binary-unit:1, _/binary>> = Data,
       ?assertEqual(D,transit:write(Val))
   end || Ext <- ["json"]].

