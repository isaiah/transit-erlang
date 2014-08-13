-module(roundtrip_tests).
-include_lib("eunit/include/eunit.hrl").

start_transit() ->
  ok = transit:start().

stop_transit(ok) ->
  ok = transit:stop().

continuous_write_test_() ->
  {foreach,
   fun start_transit/0,
   fun stop_transit/1,
   [
    fun no_cache_previous_write/1
   ]}.

no_cache_previous_write(ok) ->
  Tests = [{<<"[\"~#'\",1]">>, 1},
           {<<"[\"~#'\",1]">>, 1}
          ],
  [fun() -> Rep = transit:write(Val) end || {Rep, Val} <- Tests].
