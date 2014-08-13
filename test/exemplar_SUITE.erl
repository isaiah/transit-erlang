-module(exemplar_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([exemplar_tests/1, init_per_testcase/2, end_per_testcase/2]).

-define(datadir, "../../transit-format/examples/0.8/simple/").

all() -> [exemplar_tests].

init_per_testcase(_, Config) ->
  transit:start(),
  Config.

end_per_testcase(_, Config) ->
  transit:stop(),
  Config.

exemplar_tests(Config) ->
  Dir = ?config(data_dir, Config),
  exemplar("nil", undefined, Dir),
  exemplar("false", false, Dir),
  exemplar("true", true, Dir),
  exemplar("zero", 0, Dir),
  exemplar("one", 1, Dir),
  exemplar("one_string", "hello", Dir),
  ok.

exemplar(Name, Val, Dir) ->
  lists:map(fun(Ext) ->
                File = filename:join(Dir, Name ++ "." ++ Ext),
                ct:pal("data dir: ~s", [File]),
                {ok, Data} = file:read_file(File),
                L = bit_size(Data) - 8,
                <<D:L/binary-unit:1, _/binary>> = Data,
                D = transit:write(Val)
            end, ["json"]).
