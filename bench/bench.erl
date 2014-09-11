-module(bench).

-export([run/1, run/2]).
-export([p/1]).
-export([jsx/0, msgpack/0]).

-define(ROUNDS, 300).
-define(DATA, "../../transit-format/examples/0.8/example.json").

run(Format) ->
	run(Format, ?DATA).
	
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

p(Format) -> p(Format, ?DATA).

p(Format, File) ->
	{ok, Data} = file:read_file(File),
	WData = transit:read(Data),
	RData = transit:write(WData, [{format, Format}]),
	eprof:profile(fun() ->
		transit:read(RData, [{format, Format}])
	end),
	eprof:log("out.prof.txt"),
	eprof:analyze().

jsx() ->
	{ok, Data} = file:read_file(?DATA),
	{Timing, _} = timer:tc(fun() -> jsx(?ROUNDS, Data) end),
	Timing / ?ROUNDS.

jsx(0, _) -> ok;
jsx(N, Data) ->
	jsx:decode(Data),
	jsx(N-1, Data).
	
msgpack() ->
	{ok, Data} = file:read_file(?DATA),
	R = transit:read(Data),
	W = transit:write(R, [{format, msgpack}]),
	{Timing, _} = timer:tc(fun() -> msgpack(?ROUNDS, W) end),
	Timing / ?ROUNDS.

msgpack(0, _) -> ok;
msgpack(N, Data) ->
	msgpack:unpack(Data, [{format, jsx}]),
	msgpack(N-1, Data).
