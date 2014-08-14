-module(transit_reader).
-include_lib("transit_format.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([read/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

read(Name) ->
  Cache = {dict:new(), dict:new()},
  {Val, _Cache} = decode(Cache, jsx:decode(Name), false),
  Val.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
decode(Cache, Name, AsMapKey) when is_bitstring(Name) ->
  decode_string(Cache, Name, AsMapKey);
decode(Cache, Name, AsMapKey) when is_list(Name) ->
  case Name of
    [?MAP_AS_ARR|Tail] ->
      decode_array_hash(Cache, Tail, AsMapKey);
    [{_, _}|_] ->
      decode_hash(Cache, Name, AsMapKey);
    [EscapedTag, Rep] ->
      <<"~", "#", Tag/binary>> = EscapedTag,
      {DRep, Cache1} = decode(Cache, Rep, AsMapKey),
      {decode_tag(Tag, DRep), Cache1};
    _ ->
      decode_array(Cache, Name, AsMapKey)
  end;
decode(Cache, Name, _AsMapKey) ->
  {Name, Cache}.

decode_string(Cache, String, AsMapKey) ->
  {OrigStr, Cache1} = transit_rolling_cache:decode(Cache, String, AsMapKey),
  {parse_string(OrigStr, AsMapKey), Cache1}.

parse_string(String, _Cache) ->
  case String of
    <<"~",Tag:8/binary-unit:1, Rep/binary>> ->
      case transit_read_handlers:handler(Tag) of
        F when is_function(F) ->
          F(Rep);
        _ ->
          if Tag =:= ?ESC; Tag =:= ?SUB; Tag =:= ?RES ->
               [Tag|Rep];
             Tag =:= "#" ->
               Rep;
             true ->
               #tagged_value{tag=Tag, rep=Rep}
          end
      end;
    _ ->
      String
  end.

decode_array_hash(Cache, [Key,Val|Name], AsMapKey) ->
  {DKey, C1} = decode(Cache, Key, true),
  {DVal, C2} = decode(C1, Val, false),
  {Tail, C3} = decode_array_hash(C2, Name, AsMapKey),
  {[{DKey, DVal}|Tail], C3};
decode_array_hash(Cache, [], _AsMapKey) ->
  {[], Cache}.

decode_array(Cache, Name, AsMapKey) ->
  lists:mapfoldl(fun(El, C) ->
                     decode(C, El, AsMapKey)
                 end, Cache, Name).

decode_tag(Tag, Rep) ->
  F = transit_read_handlers:handler(Tag),
  F(Rep).

decode_hash(Cache, Name, AsMapKey) when length(Name) =:= 1 ->
  case Name of
    [{Key, Val}] ->
      case decode(Cache, Key, AsMapKey) of
        {#tagged_value{tag=Tag}, C1} ->
          {DecodedVal, C2} = decode(C1, Val, AsMapKey),
          {decode_tag(Tag, DecodedVal), C2};
        {DecodedKey, C3} ->
          {Rep, C4} = decode(C3, Val, false),
          {[{DecodedKey, Rep}], C4}
      end;
    _ ->
      erlang:throw(unidentified_read)
  end;
decode_hash(Cache, Name, _AsMapKey) ->
  lists:mapfoldl(fun({Key, Val}, C) ->
                      {DKey, C1} = decode(C, Key, true),
                      {DVal, C2} = decode(C1, Val, false),
                      {{DKey, DVal}, C2}
                 end, Cache, Name).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start_server() ->
  {dict:new(), dict:new()}.

stop_server(_C) ->
  ok.

unmarshal_test_() ->
  {foreach,
   fun start_server/0,
   fun stop_server/1,
   [fun unmarshal_quoted/1]}.

unmarshal_quoted(C) ->
  Tests = [{1, <<"[\"~#'\", 1]">>},
           {<<"foo">>, <<"[\"~#'\", \"foo\"]">>},
           {undefined, <<"[\"~#'\", null]">>},
           {true, <<"[\"~#'\", true]">>},
           {false, <<"[\"~#'\", false]">>},
           {transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~m0\"]">>},
           %{transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~t1970-01-01T00:00:01.000Z\"]">>},
           {sets:from_list([<<"foo">>, <<"bar">>, <<"baz">>]), <<"[\"~#set\", [\"foo\",\"bar\",\"baz\"]]">>},
           {[{<<"foo">>, <<"bar">>}], <<"{\"foo\":\"bar\"}">>},
           {[{<<"a">>, <<"b">>}, {3, 4}], <<"[\"^ \",\"a\",\"b\",3,4]">>},
           {[{a, b}, {3, 4}], <<"[\"^ \",\"~:a\",\"~:b\",3,4]">>},
           {[{foo, <<"bar">>}], <<"{\"~:foo\":\"bar\"}">>}
          ],
  [fun() -> {Val, _} = decode(C, jsx:decode(Str), false) end || {Val, Str} <- Tests].

parse_string_test() ->
  ?assertEqual(foo, parse_string(<<"~:foo">>, false)).
-endif.
