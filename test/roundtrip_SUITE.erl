-module(roundtrip_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([continuous_write/1]).

all() -> [continuous_write].

continuous_write(_Config) ->
  Tests = [{<<"[\"~#'\",1]">>, 1},
           {<<"[\"~#'\",1]">>, 1}
          ],
  [Rep = transit:write(Val, [{format, json}, {handler, ?MODULE}]) || {Rep, Val} <- Tests].
