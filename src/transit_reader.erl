-module(transit_reader).
-include_lib("transit_format.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([read/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

read(Obj, Config) ->
  Format = format(Config),
  Cache = transit_rolling_cache:empty(Format),
  
  case unpack(Obj, Format) of
    {ok, Rep} ->
      {Val, _} = decode(Cache, Rep, false),
      Val;
    {error, unknown_format} ->
      error(badarg)
  end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

unpack(Obj, msgpack) -> msgpack:unpack(Obj, [{format, jsx}]);
unpack(Obj, json) -> {ok, jsx:decode(Obj)};
unpack(Obj, json_verbose) -> {ok, jsx:decode(Obj)};
unpack(_Obj, undefined) -> {error, unknown_format}.

format(#{ format := F }) -> F;
format(#{}) -> undefined;
format(Config) when is_list(Config) ->
    proplists:get_value(format, Config, undefined).

decode(Cache, Str, AsMapKey) when is_binary(Str) -> decode_string(Cache, Str, AsMapKey);
decode(Cache, [?MAP_AS_ARR|Tail], AsMapKey) ->
  {L, C} = decode_array_hash(Cache, Tail, AsMapKey),
  {transit_utils:map_rep(L), C};
%% If the output is from a verbose write, it will be encoded as a hash
decode(Cache, [{Key, Val}], AsMapKey) ->
  case decode(Cache, Key, true) of
    {{tag, Tag}, C1} ->
      {DVal, C2} = decode(C1, Val, AsMapKey),
      {decode_tag(Tag, DVal), C2};
    {DKey, C1} ->
      {DVal, C2} = decode(C1, Val, false),
      {transit_utils:map_rep([{DKey, DVal}]), C2}
  end;
decode(Cache, [{_, _}|_] = Obj, AsMapKey) ->
  {L, C} = decode_hash(Cache, Obj, AsMapKey),
  {transit_utils:map_rep(L), C};
decode(Cache, [EscapedTag, Rep] = Name, AsMapKey) when is_binary(EscapedTag) ->
  {OrigTag, C} = transit_rolling_cache:decode(Cache, EscapedTag, AsMapKey),
  case OrigTag of
    <<"~", "#", Tag/binary>> ->
      {DRep, C1} = decode(C, Rep, AsMapKey),
      {decode_tag(Tag, DRep), C1};
    _ ->
      %% Abort the above and decode as an array. Note the reference back to the original cache
      decode_array(Cache, Name, AsMapKey)
  end;
decode(Cache, [{}], _AsMapKey) -> {#{}, Cache};
decode(Cache, Obj, AsMapKey) when is_list(Obj) -> decode_array(Cache, Obj, AsMapKey);
decode(Cache, null, _AsMapKey) -> {undefined, Cache};
decode(Cache, Obj, _AsMapKey) -> {Obj, Cache}.

decode_string(Cache, String, AsMapKey) ->
  {OrigStr, Cache1} = transit_rolling_cache:decode(Cache, String, AsMapKey),
  {parse_string(OrigStr), Cache1}.

parse_string(<<"~", Tag:1/binary, _/binary>> = Bin) when Tag =:= ?ESC; Tag =:= ?SUB; Tag =:= ?RES ->
  <<_, Data/binary>> = Bin,
  Data;
parse_string(<<"~#", Rep/binary>>) -> {tag, Rep};
parse_string(<<"~", Tag:1/binary, Rep/binary>>) -> handle(Tag, Rep);
parse_string(S) -> S.

decode_array_hash(Cache, [Key,Val|Name], AsMapKey) ->
  {DKey, C1} = decode(Cache, Key, true),
  {DVal, C2} = decode(C1, Val, false),
  {Tail, C3} = decode_array_hash(C2, Name, AsMapKey),
  {[{DKey, DVal}|Tail], C3};
decode_array_hash(Cache, [], _AsMapKey) ->
  {[], Cache}.

decode_array(Cache, Obj, AsMapKey) ->
  lists:mapfoldl(fun(El, C) -> decode(C, El, AsMapKey) end, Cache, Obj).

decode_tag(Tag, Rep) -> handle(Tag, Rep).

decode_hash(Cache, [{Key, Val}], AsMapKey) ->
  case decode(Cache, Key, AsMapKey) of
    {#tagged_value{tag=Tag}, C1} ->
      {DecodedVal, C2} = decode(C1, Val, AsMapKey),
      {decode_tag(Tag, DecodedVal), C2};
    {DecodedKey, C3} ->
      {Rep, C4} = decode(C3, Val, false),
      {[{DecodedKey, Rep}], C4}
  end;
decode_hash(_Cache, [_], _AsMapKey) ->
  exit(unidentified_read);
decode_hash(Cache, Name, _AsMapKey) ->
  lists:mapfoldl(fun({Key, Val}, C) ->
                      {DKey, C1} = decode(C, Key, true),
                      {DVal, C2} = decode(C1, Val, false),
                      {{DKey, DVal}, C2}
                 end, Cache, Name).

handle(?CMap, Rep) -> transit_utils:map_rep(list_to_proplist(Rep));
handle(?Null, _) -> undefined;
handle(?Boolean, "t") -> true;
handle(?Boolean, "f") -> false;
handle(?Int, Rep) -> binary_to_integer(Rep);
handle(?BigInt, Rep) -> binary_to_integer(Rep);
handle(?Float, Rep) -> binary_to_float(Rep);
handle(?QUOTE, null) -> undefined;
handle(?QUOTE, Rep) -> Rep;
handle(?Set, Rep) -> sets:from_list(Rep);
handle(?List, Rep) -> transit_types:list(Rep);
handle(?Keyword, Rep) -> binary_to_atom(Rep, utf8);
handle(?Symbol, Rep) -> transit_types:symbol(Rep);
handle(?Date, Rep) when is_integer(Rep) ->
	transit_types:datetime(transit_utils:ms_to_timestamp(Rep));
handle(?Date, Rep) ->
	transit_types:datetime(transit_utils:ms_to_timestamp(binary_to_integer(Rep)));
handle(?VerboseDate, Rep) ->
	transit_types:datetime(transit_utils:iso_8601_to_timestamp(Rep));
handle(?UUID, [_, _] = U) ->
	transit_types:uuid(list_to_binary(transit_utils:uuid_to_string(U)));
handle(?UUID, Rep) ->
	transit_types:uuid(Rep);
handle(Tag, Value) ->
	#tagged_value { tag = Tag, rep = Value }.

list_to_proplist([]) -> [];
list_to_proplist([K,V|Tail]) -> [{K,V}|list_to_proplist(Tail)].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start_server() ->
  transit_rolling_cache:empty(json).

stop_server(_C) ->
  ok.

unmarshal_test_() ->
  {foreach,
   fun start_server/0,
   fun stop_server/1,
   [fun unmarshal_quoted/1,
    fun unmarshal_extend/1]}.

unmarshal_quoted(C) ->
  Tests = [{1, <<"[\"~#'\", 1]">>},
           {<<"foo">>, <<"[\"~#'\", \"foo\"]">>},
           {undefined, <<"[\"~#'\", null]">>},
           {true, <<"[\"~#'\", true]">>},
           {false, <<"[\"~#'\", false]">>},
           {'~', <<"[\"~#'\",\"~:~\"]">>},
           {'^', <<"[\"~#'\",\"~:^\"]">>},
           {<<"~hello">>, <<"[\"~#'\",\"~~hello\"]">>},
           {transit_types:symbol(<<"hello">>), <<"[\"~#'\",\"~$hello\"]">>},
           {transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~m0\"]">>}
           %{transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~t1970-01-01T00:00:01.000Z\"]">>},
          ],
  [fun() -> {Val, _} = decode(C, jsx:decode(Str), false) end || {Val, Str} <- Tests].

unmarshal_extend(C) ->
  Tests = [{[[c, undefined]], <<"[[\"~:c\",null]]">>},
           {[], <<"[]">>},
           {#{}, <<"{}">>},
           {maps:from_list([{<<"foo">>, <<"bar">>}]), <<"{\"foo\":\"bar\"}">>},
           {maps:from_list([{<<"a">>, <<"b">>}, {3, 4}]), <<"[\"^ \",\"a\",\"b\",3,4]">>},
           {maps:from_list([{a, b}, {3, 4}]), <<"[\"^ \",\"~:a\",\"~:b\",3,4]">>},
           {sets:from_list([<<"foo">>, <<"bar">>, <<"baz">>]), <<"[\"~#set\", [\"foo\",\"bar\",\"baz\"]]">>},
           {maps:from_list([{foo, <<"bar">>}]), <<"{\"~:foo\":\"bar\"}">>}],
  [fun() -> {Val, _} = decode(C, jsx:decode(Str), false) end || {Val, Str} <- Tests].

parse_string_test_() ->
  ?_assertEqual(foo, parse_string(<<"~:foo">>)),
  ?_assertEqual(<<"~foo">>, parse_string(<<"~~foo">>)),
  ?_assertEqual({tag, <<"'">>}, parse_string(<<"~#'">>)).
-endif.
