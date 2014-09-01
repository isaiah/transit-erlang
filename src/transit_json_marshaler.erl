-module(transit_json_marshaler).
-behaviour(transit_marshaler).

-include_lib("transit_format.hrl").

-export([emit_null/2, emit_boolean/2, emit_int/2, emit_float/2, emit_string/3,
         emit_object/2, emit_tagged/2, emit_encoded/3, emit_array/2, emit_map/2,
         emit_cmap/2, handler/1]).

-define(MAX_INT, 9007199254740993). % math:pow(2, 63)
-define(MIN_INT, -9007199254740993). % -math:pow(2, 63)

emit_null(_Rep, Env) ->
  case transit_marshaler:as_map_key(Env) of
    true ->
      emit_string(<<?ESC/bitstring, ?Null/bitstring>>, <<"null">>, Env);
    false ->
      emit_object(undefined, Env)
  end.

emit_boolean(Rep, Env) ->
  case transit_marshaler:as_map_key(Env) of
    true ->
      emit_string(<<?ESC/bitstring, ?Boolean/bitstring>>, Rep, Env);
    false ->
      emit_object(Rep, Env)
  end.

emit_int(Rep, Env) when is_integer(Rep), Rep =< ?MIN_INT; is_integer(Rep), Rep >= ?MAX_INT ->
  emit_string(<<?ESC/bitstring, ?Int/bitstring>>, integer_to_binary(Rep), Env);
emit_int(Rep, Env) ->
  case transit_marshaler:as_map_key(Env) of
    true ->
      emit_string(<<?ESC/bitstring, ?Int/bitstring>>, Rep, Env);
    false ->
      emit_object(Rep, Env)
  end.

emit_float(Rep, Env) ->
  case transit_marshaler:as_map_key(Env) of
    true ->
      emit_string(<<?ESC/bitstring, ?Float/bitstring>>, Rep, Env);
    false ->
      emit_object(Rep, Env)
  end.

-spec emit_tagged(Rep, Env) ->
  {Resp, Env} when Rep::tagged_value(), Resp::bitstring(), Env::transit_marshaler:env().

emit_tagged(_TaggedValue=#tagged_value{tag=Tag, rep=Rep}, S0) ->
  Cache = transit_marshaler:cache(S0),
  {EncodedTag, Cache1} = transit_rolling_cache:encode(Cache, <<?ESC/bitstring, "#", Tag/bitstring>>, transit_marshaler:as_map_key(S0)),
  S1 = S0#env{cache=Cache1},
  {Tag1, S2} = emit_object(EncodedTag, S1),
  {Body, S3} =  transit_marshaler:marshal(?MODULE, Rep, S2),
  {[Tag1, Body], S3}.

-spec emit_encoded(Tag, Rep, Env) ->
  {Rep, Env} when Tag::bitstring(), Rep::bitstring(), Env::transit_marshaler:env().

emit_encoded(Tag, Rep, Env) ->
  Handler = transit_write_handlers:handler(Rep),
  RepFun = Handler#write_handler.rep,
  StrRep = RepFun(Rep),
  emit_tagged(#tagged_value{tag=Tag, rep=StrRep}, Env).

-spec emit_string(bitstring(), bitstring(), S) ->
  {bitstring(), S} when S::transit_marshaler:env().

emit_string(Tag, String, Env) ->
  Escaped = transit_marshaler:escape(String),
  Cache = transit_marshaler:cache(Env),
  {Encoded, Cache1} = transit_rolling_cache:encode(Cache, <<Tag/bitstring, Escaped/bitstring>>, transit_marshaler:as_map_key(Env)),
  emit_object(Encoded, Env#env{cache=Cache1}).

-spec emit_object(Rep, Env) ->
  {Resp, Env} when Rep::term(), Resp::bitstring(), Env::transit_marshaler:env().
emit_object(Obj, S1) ->
  Body = if is_bitstring(Obj); is_integer(Obj); is_float(Obj) ->
              Obj;
            is_atom(Obj) ->
              case Obj of
                undefined ->
                  null;
                _ ->
                  Obj
              end;
            true ->
              case io_lib:printable_list(Obj) of
                true ->
                  transit_marshaler:quote_string(Obj);
                false ->
                  exit(unidentified_write)
              end
         end,
  {Body, S1}.

emit_map(M, S) ->
  A = [?MAP_AS_ARR|transit_marshaler:flatten_map(M)],
  {Body, S2, _} = lists:foldl(fun (E, {In, NS, AsMapKey}) ->
                               NS1 = transit_marshaler:force_as_map_key(AsMapKey, NS),
                               {NE, NS2} = transit_marshaler:marshal(?MODULE, E, NS1),
                               {[NE|In], NS2, not AsMapKey}
                           end,
                           {[], S, false}, A),
  {lists:reverse(Body), S2}.

emit_cmap(M, S) ->
  emit_tagged(#tagged_value{tag=?CMap, rep=transit_marshaler:flatten_map(M)}, S).

emit_array(A, S) ->
  {Body, S2} = lists:foldl(fun (E, {In, NS1}) ->
                        {NE, NS2} = transit_marshaler:marshal(?MODULE, E, NS1),
                        {[NE|In], NS2}
                    end,
                    {[], S}, A),
  {lists:reverse(Body), S2}.

handler(_Obj) -> undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start_server() ->
  transit_marshaler:new_env().

stop_server(Env) ->
  ok.

marshals_extention_test_() ->
  {foreach,
   fun start_server/0,
   fun stop_server/1,
   [fun marshals_tagged/1,
    fun marshals_extend/1
   ]}.

marshals_tagged(Env) ->
  Tests = [{<<"[\"~#'\",\"foo\"]">>, "foo"},
           {<<"[\"~#'\",\"foo\"]">>, <<"foo">>},
           {<<"[\"~#'\",null]">>, undefined},
           {<<"[\"~#'\",1234]">>, 1234},
           {<<"[\"~#'\",true]">>, true},
           {<<"[\"~#'\",false]">>, false},
           {<<"[\"~#'\",\"~m0\"]">>, transit_types:datetime({0,0,0})},
           {<<"[\"~#'\",2.5]">>, 2.5},
           {<<"[\"~#'\",2.998e8]">>, 2.998E8},
           {<<"[\"~#'\",9007199254740992]">>,9007199254740992},
           {<<"[\"~#'\",\"~i9007199254740993\"]">>,9007199254740993},
           {<<"[\"~#'\",\"~n1\"]">>,transit_types:bigint(1)},
           %{<<"[\"~#'\",\"~n9223372036854775806\"]">>,9223372036854775806},
           {<<"[\"~#'\",\"~~hello\"]">>, "~hello"},
           {<<"[\"~#'\",\"~$hello\"]">>, transit_types:symbol("hello")}
          ],
  [fun() -> {Raw, _} = emit_tagged(#tagged_value{tag=?QUOTE, rep=Rep}, Env),
            Res = jsx:encode(Raw)
   end || {Res, Rep} <- Tests].

marshals_extend(_Env) ->
  Tests = [{<<"[]">>, []},
           {<<"[\"^ \"]">>, [{}]},
           {<<"[\"^ \"]">>, #{}},
           %{<<"[\"\"]">>, [""]},
           {<<"[\"\"]">>, [<<"">>]},
           {<<"[\"a\",2,\"~:a\"]">>, ["a", 2, a]},
           {<<"[\"^ \",\"~:a\",\"~:b\",\"~i3\",4]">>, #{a => b, 3 => 4}},
           {<<"[\"^ \",\"a\",\"b\",\"~i3\",4]">>, #{"a" => "b", 3 => 4}},
           {<<"[\"^ \",\"~:a\",\"~:b\",\"~i3\",4]">>, [{a, b}, {3, 4}]},
           {<<"[\"^ \",\"a\",\"b\",\"~i3\",4]">>, [{"a", "b"}, {3, 4}]},
           {<<"[[\"^ \",\"foobar\",\"foobar\"],[\"^ \",\"^0\",\"foobar\"]]">>,
             [#{"foobar" =>"foobar"},#{"foobar" =>"foobar"}]},
           {<<"[[\"^ \",\"~:foobar\",\"foobar\"],[\"^ \",\"^0\",\"foobar\"]]">>,
             [#{foobar =>"foobar"},#{foobar =>"foobar"}]},
           {<<"[\"~:atom-1\"]">>, ['atom-1']},
           {<<"[\"~~hello\"]">>, ["~hello"]},
           {<<"[\"~rhttp://google.com\"]">>, [transit_types:uri("http://google.com")]},
           {<<"[\"~u531a379e-31bb-4ce1-8690-158dceb64be6\"]">>, [transit_types:uuid("531a379e-31bb-4ce1-8690-158dceb64be6")]},
           {<<"[\"~#'\",\"~m0\"]">>, transit_types:datetime({0,0,0})},
           {<<"[\"~bc3VyZS4=\"]">>, [transit_types:binary("c3VyZS4=")]},
           {<<"[\"~#set\",[\"baz\",\"foo\",\"bar\"]]">>, sets:from_list(["foo", "bar", "baz"])}
          ],
  [fun() -> Res = jsx:encode(transit_marshaler:marshal_top(?MODULE, Rep, {json, ?MODULE})) end || {Res, Rep} <- Tests].
-endif.
