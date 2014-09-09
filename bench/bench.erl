-module(bench).

-export([run/0, run/1]).

-define(ROUNDS, 1000).

run() ->
	run("../../transit-format/examples/0.8/example.json").
	
run(File) ->
	{ok, RData} = file:read_file(File),
	WData = transit:read(RData),
	io:format("Read~n"),
	{ReadTiming, ok} = timer:tc(fun() -> run_read(?ROUNDS, RData) end),
	io:format("Write~n"),
	{WriteTiming, ok} = timer:tc(fun() -> run_write(?ROUNDS, WData) end),
	io:format("ISO~n"),
	{IsoTiming, ok} = timer:tc(fun() -> run_iso(?ROUNDS, RData) end),
	#{
	  read => ReadTiming / ?ROUNDS,
	  write => WriteTiming / ?ROUNDS,
	  iso => IsoTiming / ?ROUNDS
	}.
	

run_read(0, _Data) -> ok;
run_read(N, Data) ->
	transit:read(Data),
	run_read(N-1, Data).
	
run_write(0, _Data) -> ok;
run_write(N, Data) ->
	transit:write(Data),
	run_write(N-1, Data).

run_iso(0, _Data) -> ok;
run_iso(N, Data) ->
	Read = transit:read(Data),
	transit:write(Read),
	run_iso(N-1, Data).
