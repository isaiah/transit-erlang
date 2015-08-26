-module(exemplar_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, suite/0]).

%%% exemplar tests
-export([
         doubles_interesting_exemplar/1,
         doubles_small_exemplar/1,
         false_exemplar/1,
         ints_exemplar/1,
         ints_interesting_exemplar/1,
         ints_interesting_neg_exemplar/1,
         keywords_exemplar/1,
         list_empty_exemplar/1,
         list_mixed_exemplar/1,
         list_nested_exemplar/1,
         list_simple_exemplar/1,
         map_10_items_exemplar/1,
         map_mixed_exemplar/1,
         map_nested_exemplar/1,
         map_nested_exemplars/1,
         map_numeric_keys_exemplar/1,
         maps_four_char_string_keys_exemplar/1,
         maps_four_char_sym_keys_exemplar/1,
         map_simple_exemplar/1,
         maps_three_char_string_keys_exemplar/1,
         maps_three_char_sym_keys_exemplar/1,
         map_string_keys_exemplar/1,
         maps_two_char_string_keys_exemplar/1,
         maps_two_char_sym_keys_exemplar/1,
         maps_unrecognized_keys_exemplar/1,
         map_unrecognized_vals_exemplar/1,
         map_vector_keys_exemplar/1,
         nil_exemplar/1,
         one_date_exemplar/1,
         one_exemplar/1,
         one_keyword_exemplar/1,
         one_string_exemplar/1,
         one_symbol_exemplar/1,
         one_uri_exemplar/1,
         one_uuid_exemplar/1,
         set_empty_exemplar/1,
         set_mixed_exemplar/1,
         set_nested_exemplar/1,
         set_simple_exemplar/1,
         small_ints_exemplar/1,
         small_strings_exemplar/1,
         strings_hash_exemplar/1,
         strings_hat_exemplar/1,
         strings_tilde_exemplar/1,
         symbols_exemplar/1,
         true_exemplar/1,
         uris_exemplar/1,
         uuids_exemplar/1,
         vector_1935_keywords_repeated_twice_exemplar/1,
         vector_1936_keywords_repeated_twice_exemplar/1,
         vector_1937_keywords_repeated_twice_exemplar/1,
         vector_empty_exemplar/1,
         vector_mixed_exemplar/1,
         vector_nested_exemplar/1,
         vector_simple_exemplar/1,
         vector_unrecognized_vals_exemplar/1,
         vector_special_numbers/1,
         zero_exemplar/1

        ]).


-define(datadir, "../../transit-format/examples/0.8/simple/").

-define(ArraySimple, [1,2,3]).
-define(ArrayMixed, [0, 1, 2.0, true, false, <<"five">>, {kw, <<"six">>}, transit_types:symbol(<<"seven">>), <<"~eight">>, undefined]).
-define(ArrayNested, [?ArraySimple, ?ArrayMixed]).
-define(SmallStrings, [<<>>, <<"a">>, <<"ab">>, <<"abc">>, <<"abcd">>, <<"abcde">>, <<"abcdef">>]).
-define(SetSimple, sets:from_list(?ArraySimple)).
-define(SetMixed, sets:from_list(?ArrayMixed)).
-define(SetNested, sets:from_list([sets:from_list(?ArraySimple), sets:from_list(?ArrayMixed)])).

-define(ListSimple, transit_types:list(?ArraySimple)).
-define(ListMixed, transit_types:list(?ArrayMixed)).
-define(ListNested, transit_types:list([transit_types:list(?ArraySimple), transit_types:list(?ArrayMixed)])).

-define(MapSimple, #{{kw, <<"a">>} => 1, {kw, <<"b">>}  => 2, {kw, <<"c">>} => 3}).
-define(MapMixed, #{{kw, <<"a">>} => 1, {kw, <<"b">>} => <<"a string"/utf8>>, {kw, <<"c">>} => true}).
-define(MapNested, #{{kw, <<"simple">>} => ?MapSimple, {kw, <<"mixed">>}  => ?MapMixed}).
%-define(MapSimple, [{{kw, <<"a">>},1},{{kw, <<"b">>},2},{{kw, <<"c">>},3}]).
%-define(MapMixed, [{{kw, <<"a">>},1}, {{kw, <<"b">>}, <<"a string"/utf8>>}, {{kw, <<"c">>}, true}]).
%-define(MapNested, [{{kw, <<"simple">>}, ?MapSimple}, {{kw, <<"mixed">>}, ?MapMixed}]).

-define(POWER_OF_TWO, lists:map(fun(X) -> erlang:round(math:pow(2, X)) end, lists:seq(0, 65))).
-define(INTERESTING_INTS, lists:flatten(lists:map(fun(X) -> lists:seq(X -2, X + 2) end, ?POWER_OF_TWO))).

-define(UUIDS, [transit_types:uuid(<<"5a2cbea3-e8c6-428b-b525-21239370dd55">>),
                transit_types:uuid(<<"d1dc64fa-da79-444b-9fa4-d4412f427289">>),
                transit_types:uuid(<<"501a978e-3a3e-4060-b3be-1cf2bd4b1a38">>),
                transit_types:uuid(<<"b3ba141a-a776-48e4-9fae-a28ea8571f58">>)]).

-define(URIS, [transit_types:uri(<<"http://example.com">>),
               transit_types:uri(<<"ftp://example.com">>),
               transit_types:uri(<<"file:///path/to/file.txt">>),
               transit_types:uri(<<"http://www.詹姆斯.com/"/utf8>>)]).

-define(DATES, lists:map(fun(X) -> transit_types:datetime(X) end,
                         [{-6106,017600,0}, {0,0,0}, {946,728000,0}, {1396,909037,0}])).

%-define(SYM_STRS, [<<"a">>, <<"ab">>, <<"abc">>, <<"abcd">>, <<"abcde">>, <<"a1">>, <<"b2">>, <<"c3">>, <<"a_b">>]).
-define(ATOMS, [a, ab, abc, abcd, abcde, a1, b2, c3, 'a_b']).
-define(KEYWORDS, lists:map(fun(X) -> {kw, atom_to_binary(X, utf8)} end, ?ATOMS)).
-define(SYMBOLS, lists:map(fun(X) -> transit_types:symbol(list_to_binary(atom_to_list(X))) end, ?ATOMS)).

suite() ->
    [{timetrap, {seconds, 20}}].

groups() -> [
    {ones, [parallel],
        [nil_exemplar,
               false_exemplar,
               true_exemplar,
               zero_exemplar,
               one_uri_exemplar,
               one_exemplar,
               one_string_exemplar,
               one_keyword_exemplar,
               one_symbol_exemplar,
               one_date_exemplar]},
          {vectors, [parallel],
              [vector_simple_exemplar,
               vector_empty_exemplar,
               vector_mixed_exemplar,
               vector_nested_exemplar,
               vector_special_numbers,
               small_strings_exemplar,
               strings_tilde_exemplar,
               strings_hash_exemplar,
               strings_hat_exemplar,
               ints_exemplar,
               small_ints_exemplar,
               %ints_interesting_exemplar,
               %ints_interesting_neg_exemplar,
               doubles_small_exemplar,
               doubles_interesting_exemplar,
               one_uuid_exemplar,
              uris_exemplar,
              symbols_exemplar,
              keywords_exemplar,
              uuids_exemplar]},
          {composite_extensions, [parallel],
              [list_simple_exemplar,
               list_empty_exemplar,
               list_mixed_exemplar,
               list_nested_exemplar,
               set_simple_exemplar,
               set_empty_exemplar,
               set_mixed_exemplar,
               set_nested_exemplar,
               map_simple_exemplar,
               map_mixed_exemplar,
               map_nested_exemplar,
               map_string_keys_exemplar,
               map_numeric_keys_exemplar,
               map_vector_keys_exemplar,
               map_unrecognized_vals_exemplar,
               vector_unrecognized_vals_exemplar,
               vector_1935_keywords_repeated_twice_exemplar,
               vector_1936_keywords_repeated_twice_exemplar,
               vector_1937_keywords_repeated_twice_exemplar,
               map_10_items_exemplar,
               maps_two_char_sym_keys_exemplar,
               maps_three_char_sym_keys_exemplar,
               maps_four_char_sym_keys_exemplar,
               maps_two_char_string_keys_exemplar,
               maps_three_char_string_keys_exemplar,
               maps_four_char_string_keys_exemplar,
               maps_unrecognized_keys_exemplar,
               map_nested_exemplars
              ]} ].

all() ->
    [{group, ones},
     {group, vectors},
     {group, composite_extensions}].
nil_exemplar(Conf) ->
  exemplar("nil", undefined, Conf).
false_exemplar(Conf) ->
  exemplar("false", false, Conf).
true_exemplar(Conf) ->
  exemplar("true", true, Conf).
zero_exemplar(Conf) ->
  exemplar("zero", 0, Conf).
one_exemplar(Conf) ->
  exemplar("one", 1, Conf).
one_string_exemplar(Conf) ->
  exemplar("one_string", <<"hello">>, Conf).
one_keyword_exemplar(Conf) ->
  exemplar("one_keyword", {kw, <<"hello">>}, Conf).
one_symbol_exemplar(Conf) ->
  exemplar("one_symbol", transit_types:symbol(<<"hello">>), Conf).
one_date_exemplar(Conf) ->
  exemplar("one_date", transit_types:datetime({946,728000,0}), Conf).
vector_simple_exemplar(Conf) ->
  exemplar("vector_simple", ?ArraySimple, Conf).
vector_empty_exemplar(Conf) ->
  exemplar("vector_empty", [], Conf).
vector_mixed_exemplar(Conf) ->
  exemplar("vector_mixed", ?ArrayMixed, Conf).
vector_nested_exemplar(Conf) ->
  exemplar("vector_nested", ?ArrayNested, Conf).
small_strings_exemplar(Conf) ->
  exemplar("small_strings", ?SmallStrings, Conf).
strings_tilde_exemplar(Conf) ->
  exemplar("strings_tilde", lists:map(fun(X) -> <<"~", X/binary>> end, ?SmallStrings), Conf).
strings_hash_exemplar(Conf) ->
  exemplar("strings_hash", lists:map(fun(X) -> <<"#", X/binary>> end, ?SmallStrings), Conf).
strings_hat_exemplar(Conf) ->
  exemplar("strings_hat", lists:map(fun(X) -> <<"^", X/binary>> end, ?SmallStrings), Conf).
ints_exemplar(Conf) ->
  exemplar("ints", lists:seq(0,127), Conf).
small_ints_exemplar(Conf) ->
  exemplar("small_ints", lists:seq(-5, 5), Conf).
ints_interesting_exemplar(Conf) ->
  exemplar("ints_interesting", ?INTERESTING_INTS, Conf).
ints_interesting_neg_exemplar(Conf) ->
  %ct:pal("~p~n", [lists:map(fun(X) -> -X end, ?INTERESTING_INTS)]),
  exemplar("ints_interesting_neg", lists:map(fun(X) -> -X end, ?INTERESTING_INTS), Conf).
doubles_small_exemplar(Conf) ->
  exemplar("doubles_small", lists:map(fun(X) -> float(X) end, lists:seq(-5, 5)), Conf).
doubles_interesting_exemplar(Conf) ->
  exemplar("doubles_interesting", [-3.14159, 3.14159, 4.0E11, 2.998E8, 6.626E-34], Conf).
one_uuid_exemplar(Conf) ->
  exemplar("one_uuid", lists:nth(1, ?UUIDS), Conf).
uuids_exemplar(Conf) ->
  exemplar("uuids", ?UUIDS, Conf).
one_uri_exemplar(Conf) ->
  exemplar("one_uri", lists:nth(1, ?URIS), Conf).
uris_exemplar(Conf) ->
  exemplar("uris", ?URIS, Conf).
  %exemplar("dates_interesting", ?DATES, Conf),
symbols_exemplar(Conf) ->
  exemplar("symbols", ?SYMBOLS, Conf).
keywords_exemplar(Conf) ->
  exemplar("keywords", ?KEYWORDS, Conf).
list_simple_exemplar(Conf) ->
  exemplar("list_simple", ?ListSimple, Conf).
list_empty_exemplar(Conf) ->
  exemplar("list_empty", transit_types:list([]), Conf).
list_mixed_exemplar(Conf) ->
  exemplar("list_mixed", ?ListMixed, Conf).
list_nested_exemplar(Conf) ->
  exemplar("list_nested", ?ListNested, Conf).

set_simple_exemplar(Conf) ->
  exemplar("set_simple", ?SetSimple, Conf).
set_empty_exemplar(Conf) ->
  exemplar("set_empty", sets:new(), Conf).
set_mixed_exemplar(Conf) ->
  exemplar("set_mixed", ?SetMixed, Conf).
set_nested_exemplar(Conf) ->
  exemplar("set_nested", ?SetNested, Conf).

map_simple_exemplar(Conf) ->
  exemplar("map_simple", ?MapSimple, Conf).
map_mixed_exemplar(Conf) ->
  exemplar("map_mixed", ?MapMixed, Conf).
map_nested_exemplar(Conf) ->
  exemplar("map_nested", ?MapNested, Conf).
map_string_keys_exemplar(Conf) ->
  exemplar("map_string_keys", #{<<"first">> => 1, <<"second">> => 2, <<"third">> => 3}, Conf).
map_numeric_keys_exemplar(Conf) ->
  exemplar("map_numeric_keys", #{1 => <<"one">>, 2 => <<"two">>}, Conf).
map_vector_keys_exemplar(Conf) ->
  exemplar("map_vector_keys", #{[1,1] => <<"one">>, [2, 2] => <<"two">>}, Conf).

map_unrecognized_vals_exemplar(Conf) ->
  exemplar("map_unrecognized_vals", #{{kw, <<"key">>} => <<"~Unrecognized">>}, Conf).
vector_unrecognized_vals_exemplar(Conf) ->
  exemplar("vector_unrecognized_vals", [<<"~Unrecognized">>], Conf).
vector_1935_keywords_repeated_twice_exemplar(Conf) ->
  exemplar("vector_1935_keywords_repeated_twice", array_of_keywords(1934, 1934*2), Conf).
vector_1936_keywords_repeated_twice_exemplar(Conf) ->
  exemplar("vector_1936_keywords_repeated_twice", array_of_keywords(1935, 1935*2), Conf).
vector_1937_keywords_repeated_twice_exemplar(Conf) ->
  exemplar("vector_1937_keywords_repeated_twice", array_of_keywords(1936, 1936*2), Conf).
vector_special_numbers(Conf) ->
  exemplar("vector_special_numbers", [nan, infinity, neg_infinity], Conf).

map_10_items_exemplar(Conf) ->
  exemplar("map_10_items", hash_of_size(10), Conf).
maps_two_char_sym_keys_exemplar(Conf) ->
  exemplar("maps_two_char_sym_keys", [#{{kw, <<"aa">>} => 1, {kw, <<"bb">>} => 2},
                                     #{{kw, <<"aa">>} => 3, {kw, <<"bb">>} => 4},
                                     #{{kw, <<"aa">>} => 5, {kw, <<"bb">>} => 6}],
           Conf).
maps_three_char_sym_keys_exemplar(Conf) ->
  exemplar("maps_three_char_sym_keys", [#{{kw, <<"aaa">>} => 1, {kw, <<"bbb">>} => 2},
                                        #{{kw, <<"aaa">>} => 3, {kw, <<"bbb">>} => 4},
                                        #{{kw, <<"aaa">>} => 5, {kw, <<"bbb">>} => 6}],
           Conf).
maps_four_char_sym_keys_exemplar(Conf) ->
  exemplar("maps_four_char_sym_keys", [#{{kw, <<"aaaa">>} => 1, {kw, <<"bbbb">>} => 2},
                                       #{{kw, <<"aaaa">>} => 3, {kw, <<"bbbb">>} => 4},
                                       #{{kw, <<"aaaa">>} => 5, {kw, <<"bbbb">>} => 6}],
           Conf).
maps_two_char_string_keys_exemplar(Conf) ->
  exemplar("maps_two_char_string_keys", [#{<<"aa">> => 1, <<"bb">> => 2},
                                         #{<<"aa">> => 3, <<"bb">> => 4},
                                         #{<<"aa">> => 5, <<"bb">> => 6}],
           Conf).
maps_three_char_string_keys_exemplar(Conf) ->
  exemplar("maps_three_char_string_keys", [#{<<"aaa">> => 1, <<"bbb">> => 2},
                                           #{<<"aaa">> => 3, <<"bbb">> => 4},
                                           #{<<"aaa">> => 5, <<"bbb">> => 6}],
           Conf).
maps_four_char_string_keys_exemplar(Conf) ->
  exemplar("maps_four_char_string_keys", [#{<<"aaaa">> => 1, <<"bbbb">> => 2},
                                          #{<<"aaaa">> => 3, <<"bbbb">> => 4},
                                          #{<<"aaaa">> => 5, <<"bbbb">> => 6}],
           Conf).
maps_unrecognized_keys_exemplar(Conf) ->
  exemplar("maps_unrecognized_keys",
  	       [transit_types:tv(<<"abcde">>, {kw, <<"anything">>}),
	        transit_types:tv(<<"fghij">>, {kw, <<"anything-else">>})],
          Conf).

map_nested_exemplars(Conf) ->
  lists:foreach(fun(N) ->
                    exemplar(lists:flatten(io_lib:format("map_~p_nested", [N])),
                             #{{kw, <<"f">>} => hash_of_size(N),
                               {kw, <<"s">>} => hash_of_size(N)},
                             Conf)
                end, [10, 1935, 1936, 1937]).

equals(X, X) -> true;
equals(X, Y) -> {X, '/=', Y}.

exemplar(Name, Val, Config) ->
    Dir = ?config(data_dir, Config),
    exemplar(Name, Val, Dir, {json, ".json"}),
    exemplar(Name, Val, Dir, {json_verbose, ".verbose.json"}),
    exemplar(Name, Val, Dir, {msgpack, ".mp"}),
    ok.

exemplar(Name, Expected, Dir, {Format, Ext}) ->
    FileName = filename:join(Dir, Name ++ Ext),
    {ok, Data} = file:read_file(FileName),
    %% Test reading
    Val = transit:read(Data, #{ format => Format }),
    case equals(Val, Expected) of
        true ->
            %% Test reencode
            S = transit:write(Val, #{ format => Format }),
            equals(Val, transit:read(S, #{ format => Format }));
        Fail ->
            ct:fail(Fail)
    end.

%%% generate atoms, aka keyword
-spec array_of_atoms(M,N) ->
  L when M::integer(), N::integer(), L::list().
array_of_atoms(M, N) ->
  Seeds = lists:map(fun(X) ->
                        list_to_atom(lists:flatten(io_lib:format("key~4..0B", [X])))
                    end, lists:seq(0,M)),
  if N =< M ->
       lists:sublist(Seeds, N);
     true ->
       lists:foldl(fun(_, Acc) ->
                       Seeds ++ Acc
                   end, lists:sublist(Seeds, N rem M), lists:seq(1, N div M))
  end.

array_of_keywords(M, N) ->
    [{kw, atom_to_binary(A, utf8)} || A <- array_of_atoms(M, N)].

hash_of_size(N) ->
  maps:from_list(lists:zip(array_of_keywords(N, N), lists:seq(0, N-1))).
