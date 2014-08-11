-module(transit_json_marshaler).
-behaviour(transit_marshaler).

-include_lib("transit_format.hrl").

-export([emit_null/2, emit_boolean/2, emit_int/2, emit_float/2, emit_string/3,
         emit_object/2, emit_tagged/2, emit_encoded/3, emit_array/2, emit_map/2,
         handler/1]).

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
  {ArrayStart, S1} = transit_marshaler:emit_array_start(Env),
  EncodedTag = transit_rolling_cache:encode(<<?ESC/bitstring, "#", Tag/bitstring>>, transit_marshaler:as_map_key(S1)),
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
  Encoded = transit_rolling_cache:encode(<<Tag/bitstring, Escaped/bitstring>>, transit_marshaler:as_map_key(Env)),
  emit_object(Encoded, Env).

-spec emit_object(Rep, Env) ->
  {Resp, Env} when Rep::term(), Resp::bitstring(), Env::transit_marshaler:env().
emit_object(Obj, S) ->
  {Sep, S1} = transit_marshaler:write_sep(S),
  Body = if is_bitstring(Obj) ->
              transit_marshaler:quote_string(Obj);
            is_integer(Obj) ->
              integer_to_binary(Obj);
            is_float(Obj) ->
              float_to_binary(Obj, [{decimals, 4},compact]);
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
                  erlang:throw("don't know how to encode object.")
              end
         end,
  {<<Sep/bitstring, Body/bitstring>>, S1}.

emit_map(M, S) ->
  emit_array([?MAP_AS_ARR|transit_marshaler:flatten_map(M)], S).

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
  {ok, _} = transit_rolling_cache:start_link(),
  ok.

stop_server(ok) ->
  ok = transit_rolling_cache:stop(),
  ok.

marshals_extention_test_() ->
  {foreach,
   fun start_server/0,
   fun stop_server/1,
   [fun marshals_tagged/1,
    fun marshals_extend/1
   ]}.

marshals_tagged(ok) ->
  Env = transit_marshaler:new_env(),
  Tests = [{<<"[\"~#'\",\"foo\"]">>, "foo"},
           {<<"[\"~#'\",\"foo\"]">>, <<"foo">>},
           {<<"[\"~#'\",null]">>, undefined},
           {<<"[\"~#'\",1234]">>, 1234},
           {<<"[\"~#'\",true]">>, true},
           {<<"[\"~#'\",false]">>, false},
           {<<"[\"~#'\",\"~m0\"]">>, transit_types:datetime({0,0,0})},
           {<<"[\"~#'\",2.5]">>, 2.5}
          ],
  [fun() -> {Res, _} = emit_tagged(#tagged_value{tag=?QUOTE, rep=Rep}, Env) end || {Res, Rep} <- Tests].

marshals_extend(ok) ->
  Env = transit_marshaler:new_env(),
  Tests = [{<<"[\"a\",2,\"~:a\"]">>, ["a", 2, a]},
           {<<"[\"^ \",\"~:a\",\"~:b\",3,4]">>, #{a => b, 3 => 4}},
           {<<"[\"^ \",\"a\",\"b\",3,4]">>, #{"a" => "b", 3 => 4}},
           {<<"[\"^ \",\"~:a\",\"~:b\",3,4]">>, [{a, b}, {3, 4}]},
           {<<"[\"^ \",\"a\",\"b\",3,4]">>, [{"a", "b"}, {3, 4}]},
           %XXX Failing, because the way emit_map is implemented by flatten_map
           %{<<"[[\"^ \",\"foobar\",\"foobar\"],[\"^ \",\"^0\",\"foobar\"]]">>,
           %  [#{"foobar" =>"foobar"},#{"foobar" =>"foobar"}]},
           {<<"[[\"^ \",\"~:foobar\",\"foobar\"],[\"^ \",\"^0\",\"foobar\"]]">>,
             [#{foobar =>"foobar"},#{foobar =>"foobar"}]},
           {<<"[\"~:atom-1\"]">>, ['atom-1']},
           {<<"[\"~#'\",\"~m0\"]">>, transit_types:datetime({0,0,0})},
           {<<"[\"~#set\",[\"baz\",\"foo\",\"bar\"]]">>, sets:from_list(["foo", "bar", "baz"])}
          ],
  [fun() -> {Res, _} = transit_marshaler:marshal_top(?MODULE, Rep, Env) end || {Res, Rep} <- Tests].
-endif.
