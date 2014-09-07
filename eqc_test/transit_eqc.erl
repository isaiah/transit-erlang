-module(transit_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

%% Generate a valid atom in Erlang land (we can't handle utf8 yet here before R18

simple_atom() ->
    elements([a, ab, abc, abcd, abcde, abcdef, abcdefg, abcdefghij, 'abcdef123__^~:']).
    
advanced_atom() ->
	oneof([
	  elements(['', '~^', '~u', '~n', '^ ', '^1', '^az', '$', '~$', '^$!~']),
	  elements(
	  ['d 6y_>\b\007',':\007=&1CUH','o!,:T(\\U','bl\r\037n\0222w',
	   '\031\b\vFt8/D','P-\005kaUVK','\002-F<\vS\0320',
	   '\026,g\027!WOP','x\b\bPIi9F','\007{\002\037yKcB']),
      ?LET(C, choose(0, 127), list_to_atom([C])),
	  elements([phineas, ferb, candace, doofenschmirtz, isabella, perry, major_monogram])
	]).

atom() ->
  ?SHRINK(oneof([simple_atom(), advanced_atom()]),
    [simple_atom()]).

time_point() ->
    {nat(), choose(0, 1000*1000 - 1), choose(0, 1000*1000 - 1)}.

time_point_ms() ->
    ?LET({Mega, Secs, Micros}, time_point(),
         {Mega, Secs, (Micros div 1000) * 1000}).

null() -> return(undefined).

keyword() -> atom().

symbol() ->
    ?LETSHRINK([Sym], [oneof([atom(), eqc_lib:utf8_string()])],
        transit_types:symbol(Sym)).

large_integer() -> choose(9007199254740992, 9007199254740992*9007199254740992).

integer() ->
  ?SUCHTHAT(I, oneof([int(), eqc_lib:interesting_int(), largeint()]),
    I /= -576460752303423488).

set(G) ->
    ?LET(L, list(G),
         sets:from_list(L)).

transit_time() ->
    ?SHRINK(?LET(TP, time_point_ms(), transit_types:datetime(TP)), [null()]).

transit_uuid() ->
    ?SHRINK(?LET(UUID, eqc_lib:uuid(), transit_types:uuid(UUID)), [null()]).

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
        {N, ?LAZY(
          ?LET(K, nat(),
            ?LET(L, transit_l(K+1, N, fun transit/1),
              t_shrink(L))))},
        {N, ?LAZY(
          ?LET(K, nat(),
            ?LET(L, transit_l(K+1, N, fun transit/1),
              t_shrink(sets:from_list(L)))))},
        {N, ?LAZY(?LET(K, nat(),
                    ?LETSHRINK([Ks, Vs], [transit_l(K+1, N div 2, fun transit/1),
                                          transit_l(K+1, N div 2, fun transit/1)],
                      maps:from_list(zip(Ks, Vs)))))}
    ]).

zip([] , _Bs) -> [];
zip(_As, [])  -> [];
zip([A|As], [B|Bs]) -> [{A, B} | zip(As, Bs)].

transit_l(0, _N, _G) -> [];
transit_l(1, N, G) -> ?LET(E, G(N), [E]);
transit_l(Sz, N, G) ->
  ?LETSHRINK([L, R], [transit_l(Sz div 2, N div 2, G), transit_l(Sz div 2, N div 2, G)],
    L ++ R).

  
t_shrink(T) -> ?SHRINK(T, [null()]).

%% term/0 constructs a transit term
%% Currently, the strategy for creating transit terms are not perfect w.r.t shrinking. What you want to do is to create binary trees
%% of terms which you then flatten into lists whenever you have a list-like construction (Array, Set, Map, List, …). That way, we can
%% define a shrinker which can throw away parts of the tree giving fast logarithmic shrinkage and removal. Furthermore, if you have
%% a map of the form #{X => Y} it can be shrunk to X and Y respectively, so there is a way to unwrap the map in question. All of this
%% is not done yet.      
term() ->
    ?SIZED(Size, transit(Size)).

term_size(L) when is_list(L) -> lists:sum([term_size(E) || E <- L]);
term_size(M) when is_map(M) ->
    lists:sum([term_size(E) || E <- maps:keys(M)])
  + lists:sum([term_size(E) || E <- maps:values(M)]);
term_size(X) ->
  case sets:is_set(X) of
    true ->
      lists:sum([term_size(E) || E <- sets:to_list(X)]);
    false ->
      1
  end.

term_type(null) -> [null];
term_type(true) -> [bool];
term_type(false) -> [bool];
term_type(B) when is_binary(B) -> [string];
term_type(A) when is_atom(A) -> [keyword];
term_type(I) when is_integer(I) -> [int];
term_type(F) when is_float(F) -> [float];
term_type(L) when is_list(L) ->
  Underlying = lists:flatten([term_type(K) || K <- L]),
  lists:usort([list | Underlying]);
term_type(M) when is_map(M) ->
  Keys = lists:flatten([term_type(K) || K <- maps:keys(M)]),
  Values = lists:flatten([term_type(K) || K <- maps:values(M)]),
  lists:usort([map] ++ Keys ++ Values);
term_type({tagged_value, <<"u">>, _}) -> [uuid];
term_type({tagged_value, <<"$">>, _}) -> [symbol];
term_type({transit_datetime,_}) -> [time];
term_type(Ty) ->
  case sets:is_set(Ty) of
    true ->
      Elems = lists:flatten([term_type(E) || E <- sets:to_list(Ty)]),
      lists:usort([set | Elems]);
    false ->
      [unknown]
  end.

iso(F, T) ->
    Data = transit:write(T, [{format, F}]),
    T2 = transit:read(Data, [{format, F}]),
    collect(with_title(log_10_size), trunc(math:log10(term_size(T))),
      aggregate(with_title(term_type), term_type(T),
        equals(T, T2))).

prop_iso_json() -> ?FORALL(T, term(), iso(json, T)).
prop_iso_json_verbose() -> ?FORALL(T, term(), iso(json_verbose, T)).
prop_iso_msgpack() -> ?FORALL(T, term(), iso(msgpack, T)).

prop_integer() -> ?FORALL(I, integer(), iso(json, I)).
prop_uuid() -> ?FORALL(UUID, transit_uuid(), iso(json, UUID)).
prop_string() -> ?FORALL(S, eqc_lib:utf8_string(), iso(json, S)).
prop_keyword() -> ?FORALL(KW, keyword(), iso(json, KW)).
prop_symbol() -> ?FORALL(S, symbol(), iso(json, S)).

prop_timestamp_json() ->
  ?FORALL(TS, transit_time(),
    iso(json, TS)).
prop_timestamp_json_verbose() ->
  ?FORALL(TS, transit_time(),
    iso(json_verbose, TS)).
prop_timestamp_msgpack() ->
  ?FORALL(TS, transit_time(),
    iso(msgpack, TS)).

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
    eqc:testing_time(eval_time(T, Unit), ?FORALL(X, term(), iso(Format, X))).

eval_time(N, h) -> eval_time(N*60, min);
eval_time(N, min) -> eval_time(N*60, sec);
eval_time(N, sec) -> N.

r(What, T) ->
    eqc:quickcheck(t(What, T)).

