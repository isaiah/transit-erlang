-module(transit_write_handlers).
-behaviour(transit_write_handler).
-export([handler/1]).
-export([tag/1]).
-include_lib("transit_format.hrl").
-include_lib("transit_types.hrl").

-define(UUID_MASK, math:pow(2, 64) - 1).

%%% undefined handler
undefined_tag(_) ->
  ?Null.

undefined_rep(_) ->
  undefined.

undefined_string_rep(_) ->
  <<"null">>.

%%% Integer handler
integer_tag(_I) ->
  ?Int.

integer_rep(I) ->
  I.

integer_string_rep(I) ->
  list_to_binary(integer_to_list(I)).

%%% Float handler
float_tag(_) ->
  ?Float.
float_rep(F) ->
  F.
float_string_rep(F) ->
  transit_utils:double_to_binary(F).

%%% binary handler
binary_tag(_) ->
  ?String.
binary_rep(S) ->
  S.
binary_string_rep(S) ->
  S.


%%% Boolean handler
boolean_tag(_) ->
  ?Boolean.
boolean_rep(B) ->
  B.
boolean_string_rep(B) ->
  case B of
    true ->
      <<"t">>;
    false ->
      <<"f">>
  end.

%%% Array (which is list in erlang) handler
array_tag(_A) ->
  ?Array.
array_rep(A) ->
  A.
array_string_rep(_A) ->
  undefined.

%%% Map handler
map_tag(_M) ->
  ?Map.
map_rep(M) ->
  M.
map_string_rep(_M) ->
  undefined.

%%% Keyword handler
keyword_tag(_K) ->
  ?Keyword.
keyword_rep(K) ->
  K.
keyword_string_rep(K) -> atom_to_binary(K, utf8).

%%% date handler
datetime_tag(_D) ->
  ?Date.
datetime_rep(_T=#transit_datetime{timestamp=Timestamp}) ->
  transit_utils:timestamp_to_ms(Timestamp).
datetime_string_rep(D=#transit_datetime{}) ->
  integer_to_binary(datetime_rep(D)).

handler([]) ->
  #write_handler{tag = fun array_tag/1, rep = fun array_rep/1, string_rep = fun array_string_rep/1};
handler([{}]) ->
  #write_handler{tag = fun map_tag/1, rep = fun map_rep/1, string_rep = fun map_string_rep/1};
handler(Data) when is_boolean(Data) ->
  #write_handler{tag = fun boolean_tag/1, rep = fun boolean_rep/1, string_rep = fun boolean_string_rep/1};
handler(Data) when is_float(Data) ->
  #write_handler{tag = fun float_tag/1, rep = fun float_rep/1, string_rep = fun float_string_rep/1};
handler(Data) when is_integer(Data) ->
  #write_handler{tag = fun integer_tag/1, rep = fun integer_rep/1, string_rep = fun integer_string_rep/1};
handler(Data) when is_list(Data) ->
      #write_handler{tag = fun array_tag/1, rep = fun array_rep/1, string_rep = fun array_string_rep/1};
handler(Data) when is_binary(Data) ->
  #write_handler{tag = fun binary_tag/1, rep = fun binary_rep/1, string_rep = fun binary_string_rep/1};
handler(Data) when is_map(Data) ->
  #write_handler{tag = fun map_tag/1, rep = fun map_rep/1, string_rep = fun map_string_rep/1};
handler(Data) when is_atom(Data) ->
  case Data of
    undefined ->
      #write_handler{tag = fun undefined_tag/1, rep = fun undefined_rep/1, string_rep = fun undefined_string_rep/1};
    _ ->
      #write_handler{tag = fun keyword_tag/1, rep = fun keyword_rep/1, string_rep = fun keyword_string_rep/1}
  end;
handler(Data) when is_record(Data, transit_datetime) ->
  #write_handler{tag=fun datetime_tag/1, rep=fun datetime_rep/1, string_rep=fun datetime_string_rep/1};
handler(_TaggedVal=#tagged_value{}) ->
  #write_handler{tag=fun(_T=#tagged_value{tag=Tag}) -> Tag end,
                        %(Tag) -> Tag end,
                 rep=fun(_T=#tagged_value{rep=Rep}) -> Rep end,
                        %(Rep) -> Rep end,
                 string_rep = fun(_T=#tagged_value{rep=Rep}) -> Rep end
                };
handler(Data) ->
  case transit_utils:is_set(Data) of
    undefined ->
      undefined;
    Set ->
      #write_handler{tag=fun(_) -> ?Set end,
                     rep=fun(Rep) -> #tagged_value{tag=?Array, rep=Set:to_list(Rep)} end,
                     string_rep=fun(_) -> undefined end}
  end.

tag(Data) ->
  case handler(Data) of
    undefined -> exit(unidentified_write);
    Handler ->
      TagHandler = Handler#write_handler.tag,
      TagHandler(Data)
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handler_test_() ->
  Tests = [{?Int, 1234},
           {?Map, #{}},
           {?Keyword, hi},
           {?Null, undefined}
          ],
  [fun() ->
      IntH = handler(Int),
      Tag = apply(IntH#write_handler.tag, [Int])
  end || {Tag, Int} <- Tests].

-endif.
