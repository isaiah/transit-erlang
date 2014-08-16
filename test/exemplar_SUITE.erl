-module(exemplar_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).

%%% exemplar tests
-export([nil_exemplar/1,
         false_exemplar/1,
         true_exemplar/1,
         zero_exemplar/1,
         one_exemplar/1,
         one_string_exemplar/1,
         one_keyword_exemplar/1,
         one_symbol_exemplar/1,
         one_date_exemplar/1,
         vector_simple_exemplar/1,
         vector_empty_exemplar/1,
         vector_mixed_exemplar/1,
         vector_nested_exemplar/1,
         small_strings_exemplar/1,
         strings_tilde_exemplar/1,
         strings_hash_exemplar/1,
         strings_hat_exemplar/1,
         ints_exemplar/1,
         small_ints_exemplar/1,
         doubles_small_exemplar/1,
         doubles_interesting_exemplar/1,
         one_uuid_exemplar/1,
         uuids_exemplar/1,
         one_uri_exemplar/1,
         uris_exemplar/1,
         symbols_exemplar/1,
         keywords_exemplar/1,
         list_simple_exemplar/1,
         list_empty_exemplar/1,
         list_mixed_exemplar/1,
         list_nested_exemplar/1,
         set_simple_exemplar/1,
         set_empty_exemplar/1,
         set_mixed_exemplar/1,
         set_nested_exemplar/1
        ]).


-define(datadir, "../../transit-format/examples/0.8/simple/").

-define(ArraySimple, [1,2,3]).
-define(ArrayMixed, [0, 1, 2.0, true, false, <<"five">>, six, transit_types:symbol("seven"), <<"~eight">>, undefined]).
-define(ArrayNested, [?ArraySimple, ?ArrayMixed]).
-define(SmallStrings, [<<>>, <<"a">>, <<"ab">>, <<"abc">>, <<"abcd">>, <<"abcde">>, <<"abcdef">>]).
-define(SetSimple, sets:from_list(?ArraySimple)).
-define(SetMixed, sets:from_list(?ArrayMixed)).
-define(SetNested, sets:from_list(?ArrayNested)).

-define(ListSimple, transit_types:list(?ArraySimple)).
-define(ListMixed, transit_types:list(?ArrayMixed)).
-define(ListNested, transit_types:list(?ArrayNested)).

-define(POWER_OF_TWO, lists:map(fun(X) -> erlang:round(math:pow(2, X)) end, lists:seq(0, 66))).
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
                         [{-6106,01760,0}, {0,0,0}, {946,72800,0}, {1396,90903,7}])).

%-define(SYM_STRS, [<<"a">>, <<"ab">>, <<"abc">>, <<"abcd">>, <<"abcde">>, <<"a1">>, <<"b2">>, <<"c3">>, <<"a_b">>]).
-define(KEYWORDS, [a, ab, abc, abcd, abcde, a1, b2, c3, 'a_b']).
-define(SYMBOLS, lists:map(fun(X) -> transit_types:symbol(list_to_binary(atom_to_list(X))) end, ?KEYWORDS)).

all() -> [nil_exemplar,
          false_exemplar,
          true_exemplar,
          zero_exemplar,
          one_exemplar,
          one_string_exemplar,
          one_keyword_exemplar,
          one_symbol_exemplar,
          one_date_exemplar,
          vector_simple_exemplar,
          vector_empty_exemplar,
          vector_mixed_exemplar,
          vector_nested_exemplar,
          small_strings_exemplar,
          strings_tilde_exemplar,
          strings_hash_exemplar,
          strings_hat_exemplar,
          ints_exemplar,
          small_ints_exemplar,
          doubles_small_exemplar,
          doubles_interesting_exemplar,
          one_uuid_exemplar,
          uuids_exemplar,
          one_uri_exemplar,
          uris_exemplar,
          symbols_exemplar,
          keywords_exemplar,
          list_simple_exemplar,
          list_empty_exemplar,
          list_mixed_exemplar,
          list_nested_exemplar,
          set_simple_exemplar,
          set_empty_exemplar,
          set_mixed_exemplar,
          set_nested_exemplar
         ].

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
  exemplar("one_keyword", hello, Conf).
one_symbol_exemplar(Conf) ->
  exemplar("one_symbol", transit_types:symbol("hello"), Conf).
one_date_exemplar(Conf) ->
  exemplar("one_date", transit_types:datetime({946,72800,0}), Conf).
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
  %exemplar("ints_interesting", ?INTERESTING_INTS, Conf),
  %exemplar("ints_interesting_neg", lists:map(fun(X) -> -X end, ?INTERESTING_INTS), Conf),
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



exemplar(Name, Val, Config) ->
  Dir = ?config(data_dir, Config),
  lists:map(fun(Ext) ->
                File = filename:join(Dir, Name ++ "." ++ atom_to_list(Ext)),
                {ok, Data} = file:read_file(File),
                L = bit_size(Data) - 8,
                <<D:L/binary-unit:1, _/binary>> = Data,
                % Test read
                Val = transit:read(D, [{format, Ext}]),
                %% Test reencode
                S = transit:write(Val, [{format,Ext}]),
                Val = transit:read(S, [{format,Ext}])
            end, [json]).
