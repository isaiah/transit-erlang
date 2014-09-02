%% jsx_eqc provides a model of JSON in jsx style in order to test JSX for correctness
-module(jsx_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

json_integer() ->
    int().
    
json_float() ->
    real().

json_number() ->
    oneof([json_integer(), json_float()]).

json_string() ->
    eqc_lib:utf8_string().
    
json_atom() ->
    elements([true, false, null]).
    
json_term() ->
    ?SIZED(N, json_term(N)).
    
json_map(G) ->
    ?LET(L, list({eqc_lib:utf8_string(), G}),
      maps:from_list(L)).

json_term(0) ->
    oneof([
      json_integer(),
      json_float(),
      json_number(),
      json_string(),
      json_atom()
    ]);
json_term(N) ->
    frequency([
      {1, json_term(0)},
      {N, ?LAZY(list(json_term(N div 2)))},
      {N, ?LAZY(json_map(json_term(N div 2)))}
    ]).

iso(T) ->
	E = jsx:encode(T),
	D = jsx:decode(E),
	D =:= T.

prop_iso() ->
    ?FORALL(T, json_term(),
      iso(T)).

prop_string_iso() ->
    ?FORALL(S, json_string(),
      iso(S)).
