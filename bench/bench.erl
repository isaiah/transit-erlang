-module(bench).

-export([run/1, run/2]).

-define(ROUNDS, 300).

run(Format) ->
	run(Format, "../../transit-format/examples/0.8/example.json").
	
run(Format, File) ->
	{ok, Data} = file:read_file(File),
	WData = transit:read(Data),
	RData = transit:write(WData, [{format, Format}]),
	io:format("Read~n"),
	{ReadTiming, ok} = timer:tc(fun() -> run_read(?ROUNDS, RData, [{format, Format}]) end),
	io:format("Write~n"),
	{WriteTiming, ok} = timer:tc(fun() -> run_write(?ROUNDS, WData, [{format, Format}]) end),
	io:format("ISO~n"),
	{IsoTiming, ok} = timer:tc(fun() -> run_iso(?ROUNDS, RData, [{format, Format}]) end),
	#{
	  read => ReadTiming / ?ROUNDS,
	  write => WriteTiming / ?ROUNDS,
	  iso => IsoTiming / ?ROUNDS
	}.
	
run_read(0, _Data, _Opts) -> ok;
run_read(N, Data, Opts) ->
	transit:read(Data, Opts),
	run_read(N-1, Data, Opts).
	
run_write(0, _Data, _Opts) -> ok;
run_write(N, Data, Opts) ->
	transit:write(Data, Opts),
	run_write(N-1, Data, Opts).

run_iso(0, _Data, _Opts) -> ok;
run_iso(N, Data, Opts) ->
	Read = transit:read(Data, Opts),
	transit:write(Read, Opts),
	run_iso(N-1, Data, Opts).
