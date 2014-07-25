-module(transit_rolling_cache_tests).
-include_lib("eunit/include/eunit.hrl").

is_cache_key_test() ->
  ?assert(transit_rolling_cache:is_cache_key("^123")),
  ?assertNot(transit_rolling_cache:is_cache_key("^ 123")),
  ?assertNot(transit_rolling_cache:is_cache_key("123")),
  ?assertNot(transit_rolling_cache:is_cache_key("")).
