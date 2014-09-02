-module(transit_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

%% Generate a valid atom in Erlang land (we can't handle utf8 yet here before R18

simple_atom() -> elements([a,b,c,d,e,f]).
advanced_atom() ->
    ?LET(L, list(choose(0,255)),
         list_to_atom(L)).

atom() ->
    ?SHRINK(advanced_atom(),
            [simple_atom()]).

time_point() ->
    {nat(), choose(0, 1000*1000 - 1), choose(0, 1000*1000 - 1)}.

time_point_ms() ->
    ?LET({Mega, Secs, Micros}, time_point(),
         {Mega, Secs, (Micros div 1000) * 1000}).

null() -> return(undefined).
%% string() -> binary().

keyword() -> atom().

symbol() ->
    ?LET(Sym, oneof([binary(), list(char()), atom()]),
        transit_types:symbol(Sym)).

large_integer() -> choose(9007199254740992, 9007199254740992*9007199254740992).

integer() ->
    oneof([
        int(),
        largeint()
    ]).


array(G) -> list(G).

set(G) ->
    ?LET(L, list(G),
         sets:from_list(L)).

transit_time() ->
    ?LET(TP, time_point_ms(),
         transit_types:datetime(TP)).

transit_map(KeyG, ValueG) ->
    ?LET(PL, list({KeyG, ValueG}),
      maps:from_list(PL)).

transit_uuid() ->
    ?LET(UUID, eqc_lib:uuid(),
         transit_types:uuid(UUID)).

transit(0) ->
    oneof([
        null(),
        eqc_lib:utf8_string(),
        integer(),
        transit_uuid(),
        transit_time(),
        symbol(),
        keyword()
    ]);
transit(N) ->
    frequency([
        {1, transit(0)},
        {N, ?LAZY(array(transit(N div 2)))},
        {N, ?LAZY(set(transit(N div 2)))},
        {N, ?LAZY(transit_map(transit(N div 6), transit(N div 6)))},
        {N, ?LAZY(list(transit(N div 2)))}
    ]).

term() ->
    ?SIZED(Size, transit(Size)).

gen_iso(Format) ->
    ?FORALL(T, term(),
        begin
            Data = transit:write(T, [{format, Format}]),
            T2 = transit:read(Data, [{format, Format}]),
            T =:= T2
        end).

prop_iso_json() -> gen_iso(json).
prop_iso_json_verbose() -> gen_iso(json_verbose).
prop_iso_msgpack() -> gen_iso(msgpack).

iso(F, T) ->
    Data = transit:write(T, [{format, F}]),
    T2 = transit:read(Data, [{format, F}]),
    T =:= T2.
      
prop_integer() -> ?FORALL(I, integer(), iso(json, I)).
prop_uuid() -> ?FORALL(UUID, transit_uuid(), iso(json, UUID)).
prop_string() -> ?FORALL(S, eqc_lib:utf8_string(), iso(json, S)).
prop_keyword() -> ?FORALL(KW, keyword(), iso(json, KW)).
prop_symbol() -> ?FORALL(S, symbol(), iso(json, S)).

ms({Mega, Secs, Micro}) -> {Mega, Secs, (Micro div 1000) * 1000}.

prop_time() ->
    ?FORALL(TP, time_point(),
      begin
          Coded = transit_utils:timestamp_to_ms(TP),
          Decoded = transit_utils:ms_to_timestamp(Coded),
          ms(Decoded) =:= ms(TP)
      end).

%% Running tests
%%

t(Format, {T, Unit}) ->
    eqc:testing_time(eval_time(T, Unit), gen_iso(Format)).

eval_time(N, h) -> eval_time(N*60, min);
eval_time(N, min) -> eval_time(N*60, sec);
eval_time(N, sec) -> N.

r(What, T) ->
    eqc:quickcheck(t(What, T)).

