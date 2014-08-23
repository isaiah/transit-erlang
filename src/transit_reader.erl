-module(transit_reader).
-include_lib("transit_format.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([read/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

read(Name, [{format, Format}|_Config]) ->
  Cache = transit_rolling_cache:empty(Format),
  Rep = case Format of
          msgpack ->
            {ok, R} = msgpack:unpack(Name, [{format, jsx}]),
            R;
          F when F == json; F == json_verbose ->
            jsx:decode(Name);
          _ ->
            error(badarg)
        end,
  {Val, _Cache} = decode(Cache, Rep, false),
  Val.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
decode(Cache, Name, AsMapKey) when is_bitstring(Name) ->
  decode_string(Cache, Name, AsMapKey);
decode(Cache, Name, AsMapKey) when is_list(Name) ->
  case Name of
    [?MAP_AS_ARR|Tail] ->
      {L, C} = decode_array_hash(Cache, Tail, AsMapKey),
      {transit_utils:map_rep(L), C};
    %%% If the output is from a verbose write, it will be encoded as a hash
    [{Key,Val}] ->
      case decode(Cache, Key, true) of
        {{tag, Tag}, C1} ->
          {DVal, C2} = decode(C1, Val, AsMapKey),
          {decode_tag(Tag, DVal), C2};
        {DKey, C1} ->
          {DVal, C2} = decode(C1, Val, false),
          {transit_utils:map_rep([{DKey, DVal}]), C2}
      end;
    [{_, _}|_] ->
      {L, C} = decode_hash(Cache, Name, AsMapKey),
      {transit_utils:map_rep(L), C};
    [EscapedTag, Rep] when is_bitstring(EscapedTag) ->
      {OrigTag, C} = transit_rolling_cache:decode(Cache, EscapedTag, AsMapKey),
      case OrigTag of
        <<"~", "#", Tag/binary>> ->
          {DRep, C1} = decode(C, Rep, AsMapKey),
          {decode_tag(Tag, DRep), C1};
        T ->
          ct:pal("~p", [Cache]),
          ct:pal("~p", [EscapedTag]),
          ct:pal("~p", [T]),
          ct:pal("~p", [Name]),
          exit({unknown_tag, T})
      end;
    _ ->
      decode_array(Cache, Name, AsMapKey)
  end;
decode(Cache, Name, _AsMapKey) when Name =:= null ->
  {undefined, Cache};
decode(Cache, Name, _AsMapKey) ->
  {Name, Cache}.

decode_string(Cache, String, AsMapKey) ->
  {OrigStr, Cache1} = transit_rolling_cache:decode(Cache, String, AsMapKey),
  {parse_string(OrigStr), Cache1}.

parse_string(String) ->
  case String of
    <<"~",Tag:8/binary-unit:1, Rep/binary>> ->
      case transit_read_handlers:handler(Tag) of
        F when is_function(F) ->
          F(Rep);
        _ ->
          if Tag =:= ?ESC; Tag =:= ?SUB; Tag =:= ?RES ->
               <<Tag/binary,Rep/binary>>;
             Tag =:= <<"#">> ->
               {tag, Rep};
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
  case transit_read_handlers:handler(Tag) of
    F when is_function(F) ->
      F(Rep);
    _ ->
      transit_types:tv(Tag, Rep)
  end.

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
      exit(unidentified_read)
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
  transit_rolling_cache:empty(json).

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
           {<<"~hello">>, <<"[\"~#'\",\"~~hello\"]">>},
           {[], <<"[]">>},
           {transit_types:symbol(<<"hello">>), <<"[\"~#'\",\"~$hello\"]">>},
           {transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~m0\"]">>},
           %{transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~t1970-01-01T00:00:01.000Z\"]">>},
           {sets:from_list([<<"foo">>, <<"bar">>, <<"baz">>]), <<"[\"~#set\", [\"foo\",\"bar\",\"baz\"]]">>},
           {maps:from_list([{<<"foo">>, <<"bar">>}]), <<"{\"foo\":\"bar\"}">>},
           {maps:from_list([{<<"a">>, <<"b">>}, {3, 4}]), <<"[\"^ \",\"a\",\"b\",3,4]">>},
           {maps:from_list([{a, b}, {3, 4}]), <<"[\"^ \",\"~:a\",\"~:b\",3,4]">>},
           {maps:from_list([{foo, <<"bar">>}]), <<"{\"~:foo\":\"bar\"}">>}
          ],
  [fun() -> {Val, _} = decode(C, jsx:decode(Str), false) end || {Val, Str} <- Tests].

parse_string_test_() ->
  ?_assertEqual(foo, parse_string(<<"~:foo">>)),
  ?_assertEqual(<<"~foo">>, parse_string(<<"~~foo">>)),
  ?_assertEqual({tag, <<"'">>}, parse_string(<<"~#'">>)).
-endif.
