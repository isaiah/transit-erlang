-module(transit_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

atom() ->
    oneof([a,b,c,d,e]).

timepoint() ->
    ?LET({Mega, Secs, Micros}, {int(), choose(0, 1000*1000 - 1), choose(0, 1000*1000 - 1)},
         {Mega, Secs, Micros}).

null() -> return(undefined).
%% string() -> binary().
string() -> oneof([<<"hello">>, <<"world">>]).

keyword() -> atom().

symbol() ->
    ?LET(Sym, oneof([binary(), list(char()), atom()]),
        transit_types:symbol(Sym)).

integer() -> int().

time() ->
    ?LET(TP, timepoint(),
         transit_types:datetime(TP)).

transit(0) ->
    oneof([
        null(),
        string(),
        integer(),
        keyword(),
        symbol()]);
transit(_N) ->
    frequency([
        {1, transit(0)}]).
%%        {N, ?LAZY(?LETSHRINK([T], [transit(N-1)],
%%            transit(T)))}]).

term() ->
    ?SIZED(Size, transit(Size)).

gen_iso(Format) ->
    ?FORALL(T, term(),
        begin
            Data = transit:write(T, [{format, Format}]),
            T2 = transit:read(Data, [{format, Format}]),
            eq(T, T2)
        end).

prop_iso_json() -> gen_iso(json).
prop_iso_json_verbose() -> gen_iso(json_verbose).
prop_iso_msgpack() -> gen_iso(msgpack).

eq(T, T) -> true;
eq(T, U) -> {T, '/=', U}.

%% Running tests
%%

t(Format, {T, Unit}) ->
    eqc:testing_time(eval_time(T, Unit), gen_iso(Format)).

eval_time(N, h) -> eval_time(N*60, min);
eval_time(N, min) -> eval_time(N*60, sec);
eval_time(N, sec) -> N.

r(What, T) ->
    eqc:quickcheck(t(What, T)).

