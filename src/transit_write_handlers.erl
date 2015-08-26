-module(transit_write_handlers).
-behaviour(transit_write_handler).
-export([handler/1]).
-export([tag/1]).

-include("transit_format.hrl").
-include("transit.hrl").

-define(UUID_MASK, math:pow(2, 64) - 1).

%%% undefined handler
undefined_tag(_) -> ?NULL.
undefined_rep(_) -> undefined.
undefined_string_rep(_) -> <<"null">>.

%%% Integer handler
integer_tag(_I) -> ?INT.
integer_rep(I) -> I.
integer_string_rep(I) -> list_to_binary(integer_to_list(I)).

%%% Float handler
float_tag(_) -> ?FLOAT.
float_rep(F) -> F.
float_string_rep(F) -> transit_utils:double_to_binary(F).

%%% binary handler
binary_tag(_) -> ?STRING.
binary_rep(S) -> S.
binary_string_rep(S) -> S.

%%% Boolean handler
boolean_tag(_) -> ?BOOLEAN.
boolean_rep(B) -> B.
boolean_string_rep(true) -> <<"t">>;
boolean_string_rep(false) -> <<"f">>.

%%% Array (which is list in erlang) handler
array_tag(_A) -> ?ARRAY.
array_rep(A) -> A.
array_string_rep(_A) -> undefined.

%%% Map handler
map_tag(_M) -> ?MAP.
map_rep(M) -> M.
map_string_rep(_M) -> undefined.

const(K) -> fun (_) -> K end.

special_number() ->
    F = fun (nan) -> <<"NaN">>;
            (infinity) -> <<"INF">>;
            (neg_infinity) -> <<"-INF">>
        end,
    #write_handler { tag = const(?SPECIAL_NUMBER),
                     rep = F,
                     string_rep = F
                   }.

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
handler({kw, _}) ->
  #write_handler {
    tag = fun({kw, _}) -> ?KEYWORD end,
    rep = fun({kw, KW}) -> {kw, KW} end,
    string_rep = fun({kw, KW}) -> KW end
  };
handler(undefined) ->
  #write_handler{tag = fun undefined_tag/1, rep = fun undefined_rep/1, string_rep = fun undefined_string_rep/1};
handler(nan) -> special_number();
handler(infinity) -> special_number();
handler(neg_infinity) -> special_number();
handler({timepoint, _}) ->
  #write_handler {
    tag = fun ({timepoint, _}) -> ?DATE end,
    rep = fun ({timepoint, TP}) -> transit_utils:timestamp_to_ms(TP) end,
    string_rep = fun ({timepoint, TP}) -> integer_to_binary(transit_utils:timestamp_to_ms(TP)) end
  };
handler({binary, _}) ->
  #write_handler {
  	tag = fun ({binary, _}) -> ?BINARY end,
  	rep = fun ({binary, Bin}) -> base64:encode(Bin) end,
  	string_rep = fun ({binary, Bin}) -> base64:encode(Bin) end
  };
handler({uuid, _}) ->
  #write_handler { tag = fun({uuid, _}) -> ?UUID end,
                   rep = fun({uuid, UUID}) -> UUID end,
                   string_rep = fun({uuid, UUID}) -> UUID end
                 };
handler({sym, _}) ->
  #write_handler { tag = fun({sym, _}) -> ?SYMBOL end,
                   rep = fun({sym, Symb}) when is_binary(Symb) -> Symb end,
                   string_rep = fun({sym, Symb}) when is_binary(Symb) -> Symb end
  };
handler({uri, _}) ->
  #write_handler { tag = fun({uri, _}) -> ?URI end,
                   rep = fun({uri, URI}) -> URI end,
                   string_rep = fun({uri, URI}) -> URI end
  };
handler({list, _}) ->
  #write_handler { tag = fun({list, _}) -> ?LIST end,
                   rep = fun({list, L}) -> L end,
                   string_rep = fun(_) -> exit({error, lists_have_no_string_rep}) end
  };
handler(#tagged_value{}) ->
  #write_handler {
  	tag = fun(#tagged_value { tag = Tag }) -> Tag end,
  	rep = fun(#tagged_value { rep = Rep}) -> Rep end,
  	string_rep = fun(#tagged_value { rep = Rep}) -> Rep end
  };
handler(Atom) when is_atom(Atom) ->
  #write_handler {
    tag = fun(_) -> ?KEYWORD end,
    rep = fun(A) -> A end,
    string_rep = fun(A) -> atom_to_binary(A, utf8) end
  };
handler({Mega, Secs, Micros})
  when is_integer(Mega),
       is_integer(Secs) andalso Secs >= 0 andalso Secs < 1000000,
       is_integer(Micros) andalso Micros >= 0 andalso Micros < 1000000 ->
  #write_handler {
    tag = fun (_) -> ?DATE end,
    rep = fun (TP) -> transit_utils:timestamp_to_ms(TP) end,
    string_rep = fun (TP) -> integer_to_binary(transit_utils:timestamp_to_ms(TP)) end
  };
handler(Data) ->
  case transit_utils:is_set(Data) of
    undefined ->
      undefined;
    Set ->
      #write_handler{tag=fun(_) -> ?SET end,
                     rep=fun(Rep) -> Set:to_list(Rep) end,
                     string_rep=fun(_) -> exit({error, sets_have_no_string_rep}) end}
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
  Tests = [{?INT, 1234},
           {?MAP, #{}},
           {?KEYWORD, {kw, <<"hi">>}},
           {?NULL, undefined}
          ],
  [fun() ->
      IntH = handler(Int),
      Tag = apply(IntH#write_handler.tag, [Int])
  end || {Tag, Int} <- Tests].

-endif.
