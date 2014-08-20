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

emit_tagged(_TaggedValue=#tagged_value{tag=Tag, rep=Rep}, Env) ->
  {ArrayStart, S0} = transit_marshaler:emit_array_start(Env),
  Cache = transit_marshaler:cache(S0),
  {EncodedTag, Cache1} = transit_rolling_cache:encode(Cache, <<?ESC/bitstring, "#", Tag/bitstring>>, transit_marshaler:as_map_key(S0)),
  S1 = S0#env{cache=Cache1},
  {Tag1, S2} = emit_object(EncodedTag, S1),
  {Body, S3} =  transit_marshaler:marshal(?MODULE, Rep, S2),
  {ArrayEnd, S4} = transit_marshaler:emit_array_end(S3),
  {<<ArrayStart/bitstring, Tag1/bitstring, Body/bitstring, ArrayEnd/bitstring>>, S4}.

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
  Env1 = Env#env{cache=Cache1},
  emit_object(Encoded, Env1).

-spec emit_object(Rep, Env) ->
  {Resp, Env} when Rep::term(), Resp::bitstring(), Env::transit_marshaler:env().
emit_object(Obj, S) ->
  {Sep, S1} = transit_marshaler:write_sep(S),
  Body = if is_bitstring(Obj) ->
              transit_marshaler:quote_string(Obj);
            is_integer(Obj) ->
              integer_to_binary(Obj);
            is_float(Obj) ->
              transit_utils:double_to_binary(Obj);
            is_atom(Obj) ->
              case Obj of
                undefined ->
                  <<"null">>;
                _ ->
                  list_to_binary(atom_to_list(Obj))
              end;
            true ->
              case io_lib:printable_list(Obj) of
                true ->
                  transit_marshaler:quote_string(Obj);
                false ->
                  exit(unidentified_write)
              end
         end,
  {<<Sep/bitstring, Body/bitstring>>, S1}.

emit_map(M, S) ->
  A = [?MAP_AS_ARR|transit_marshaler:flatten_map(M)],
  {ArrayStart, S1} = transit_marshaler:emit_array_start(S),
  {Body, S2, _} = lists:foldl(fun (E, {In, NS, AsMapKey}) ->
                               NS1 = transit_marshaler:force_as_map_key(AsMapKey, NS),
                               {NE, NS2} = transit_marshaler:marshal(?MODULE, E, NS1),
                               {<<In/bitstring, NE/bitstring>>, NS2, not AsMapKey}
                           end,
                           {<<>>, S1, false}, A),
  {ArrayEnd, S3} = transit_marshaler:emit_array_end(S2),
  {<<ArrayStart/bitstring, Body/bitstring, ArrayEnd/bitstring>>, S3}.

emit_cmap(M, S) ->
  emit_tagged(#tagged_value{tag=?CMap, rep=transit_marshaler:flatten_map(M)}, S).

emit_array(A, S) ->
  {ArrayStart, S1} = transit_marshaler:emit_array_start(S),
  {Body, S2} = lists:foldl(fun (E, {In, NS1}) ->
                        {NE, NS2} = transit_marshaler:marshal(?MODULE, E, NS1),
                        {<<In/bitstring, NE/bitstring>>, NS2}
                    end,
                    {<<>>, S1}, A),
  {ArrayEnd, S3} = transit_marshaler:emit_array_end(S2),
  {<<ArrayStart/bitstring, Body/bitstring, ArrayEnd/bitstring>>, S3}.

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
  [fun() -> {Res, _} = emit_tagged(#tagged_value{tag=?QUOTE, rep=Rep}, Env) end || {Res, Rep} <- Tests].

marshals_extend(_Env) ->
  Tests = [{<<"[]">>, []},
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
  [fun() -> Res = transit_marshaler:marshal_top(?MODULE, Rep, ?MODULE) end || {Res, Rep} <- Tests].
-endif.
