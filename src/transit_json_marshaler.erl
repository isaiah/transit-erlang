-module(transit_json_marshaler).
-behaviour(transit_marshaler).

-include("transit_format.hrl").
-include_lib("transit.hrl").

-export([emit_null/2, emit_boolean/2, emit_int/2, emit_float/2, emit_string/3,
         emit_object/2, emit_tagged/2, emit_encoded/3, emit_array/2, emit_map/2,
         emit_cmap/2, handler/1]).

-define(MAX_INT, 9007199254740993). % math:pow(2, 63)
-define(MIN_INT, -9007199254740993). % -math:pow(2, 63)

emit_null(_Rep, Env) ->
  case transit_marshaler:context(Env) of
    key ->
      emit_string(<<?ESC/bitstring, ?NULL/bitstring>>, <<"null">>, Env);
    value ->
      emit_object(undefined, Env)
  end.

emit_boolean(Rep, Env) ->
  case transit_marshaler:context(Env) of
    key ->
      emit_string(<<?ESC/bitstring, ?BOOLEAN/bitstring>>, Rep, Env);
    value ->
      emit_object(Rep, Env)
  end.

emit_int(Rep, Env) when is_integer(Rep), Rep =< ?MIN_INT; is_integer(Rep), Rep >= ?MAX_INT ->
  emit_string(<<?ESC/bitstring, ?INT/bitstring>>, integer_to_binary(Rep), Env);
emit_int(Rep, Env) ->
  case transit_marshaler:context(Env) of
    key ->
      emit_string(<<?ESC/bitstring, ?INT/bitstring>>, Rep, Env);
    value ->
      emit_object(Rep, Env)
  end.

emit_float(Rep, Env) ->
  case transit_marshaler:context(Env) of
    key ->
      emit_string(<<?ESC/bitstring, ?FLOAT/bitstring>>, Rep, Env);
    value ->
      emit_object(Rep, Env)
  end.

-spec emit_tagged(Rep, Env) -> {Resp, Env}
  when Rep :: #tagged_value{},
       Resp :: [binary()],
       Env :: transit_marshaler:env().

emit_tagged(#tagged_value{tag=Tag, rep=Rep}, S0) ->
  Cache = transit_marshaler:cache(S0),
  {EncodedTag, Cache1} = transit_rolling_cache:encode(Cache, <<?ESC/bitstring, "#", Tag/bitstring>>, transit_marshaler:context(S0)),
  S1 = S0#env{cache=Cache1},
  {Tag1, S2} = emit_object(EncodedTag, S1),
  {Body, S3} =  transit_marshaler:marshal(?MODULE, Rep, S2),
  {[Tag1, Body], S3}.

-spec emit_encoded(Tag, Rep, Env) ->
  {Rep, Env} when Tag::bitstring(), Rep::bitstring(), Env::transit_marshaler:env().

emit_encoded(Tag, Rep, Env) ->
  Handler = transit_write_handlers:handler(Rep),
  RepFun = Handler#write_handler.rep,
  emit_tagged(#tagged_value{tag=Tag, rep=RepFun(Rep)}, Env).

-spec emit_string(bitstring(), bitstring(), S) ->
  {bitstring(), S} when S::transit_marshaler:env().

emit_string(<<>>, String, Env) ->
  Escaped = transit_marshaler:escape(String),
  emit_raw_string(Escaped, Env);
emit_string(Tag, String, Env) ->
  emit_raw_string(<<Tag/binary, String/binary>>, Env).
    
emit_raw_string(Raw, Env) ->
  Cache = transit_marshaler:cache(Env),
  {Encoded, Cache1} = transit_rolling_cache:encode(Cache, Raw, transit_marshaler:context(Env)),
  emit_object(Encoded, Env#env{cache=Cache1}).
  
-spec emit_object(Rep, Env) ->
  {Resp, Env} when Rep::term(), Resp::bitstring(), Env::transit_marshaler:env().
emit_object(Obj, S1) ->
  {emit_object_(Obj), S1}.
  
emit_object_(O) when is_binary(O); is_integer(O); is_float(O) -> O;
emit_object_(undefined) -> null;
emit_object_(A) when is_atom(A) -> A;
emit_object_(O) ->
    case io_lib:printable_list(O) of
      true -> transit_marshaler:quote_string(O);
      false -> exit(unidentified_write)
    end.

flip(value) -> key;
flip(key) -> value.

emit_map(M, S) ->
  A = [?MAP_AS_ARR|transit_marshaler:flatten_map(M)],
  {Body, S2, _} = lists:foldl(fun (E, {In, NS, Kind}) ->
                               NS1 = transit_marshaler:force_context(Kind, NS),
                               {NE, NS2} = transit_marshaler:marshal(?MODULE, E, NS1),
                               {[NE|In], NS2, flip(Kind)}
                           end,
                           {[], S, value}, A),
  {lists:reverse(Body), S2}.

emit_cmap(M, S) ->
  emit_tagged(#tagged_value{tag=?CMAP, rep=transit_marshaler:flatten_map(M)}, S).

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

stop_server(_Env) ->
  ok.

marshals_extention_test_() ->
  {foreach,
   fun start_server/0,
   fun stop_server/1,
   [fun marshals_tagged/1,
    fun marshals_extend/1
   ]}.

marshals_tagged(Env) ->
  Tests = [{<<"[\"~#'\",[102,111,111]]">>, "foo"},
           {<<"[\"~#'\",\"foo\"]">>, <<"foo">>},
           {<<"[\"~#'\",null]">>, undefined},
           {<<"[\"~#'\",1234]">>, 1234},
           {<<"[\"~#'\",true]">>, true},
           {<<"[\"~#'\",false]">>, false},
           {<<"[\"~#'\",\"~m0\"]">>, {timepoint, {0,0,0}}},
           {<<"[\"~#'\",2.5]">>, 2.5},
           {<<"[\"~#'\",2.998e8]">>, 2.998e8},
           {<<"[\"~#'\",9007199254740992]">>,9007199254740992},
           {<<"[\"~#'\",\"~i9007199254740993\"]">>,9007199254740993},
           %{<<"[\"~#'\",\"~n9223372036854775806\"]">>,9223372036854775806},
           {<<"[\"~#'\",[126,104,101,108,108,111]]">>, "~hello"},
           {<<"[\"~#'\",\"~$hello\"]">>, {sym, <<"hello">>}}
          ],
  [fun() -> {Raw, _} = emit_tagged(#tagged_value{tag=?QUOTE, rep=Rep}, Env),
            Res = jsx:encode(Raw)
   end || {Res, Rep} <- Tests].

marshals_extend(_Env) ->
  A = {kw, <<"a">>},
  Tests = [{<<"[]">>, []},
           {<<"[\"^ \"]">>, [{}]},
           {<<"[\"^ \"]">>, #{}},
           %{<<"[\"\"]">>, [""]},
           {<<"[\"\"]">>, [<<"">>]},
           {<<"[\"a\",2,\"~:a\"]">>, [<<"a">>, 2, A]},
           {<<"[\"^ \",\"~:a\",\"~:b\",\"~i3\",4]">>, #{{kw, <<"a">>} => {kw, <<"b">>}, 3 => 4}},
           {<<"[\"^ \",\"a\",\"b\",\"~i3\",4]">>, #{<<"a">> => <<"b">>, 3 => 4}},
           {<<"[[\"^ \",\"foobar\",\"foobar\"],[\"^ \",\"^0\",\"foobar\"]]">>,
             [#{ <<"foobar">> => <<"foobar">>},#{ <<"foobar">> => <<"foobar">>}]},
           {<<"[[\"^ \",\"~:foobar\",\"foobar\"],[\"^ \",\"^0\",\"foobar\"]]">>,
             [#{{kw, <<"foobar">>} => <<"foobar">>},#{{kw, <<"foobar">>} => <<"foobar">>}]},
           {<<"[\"~:atom-1\"]">>, [{kw, <<"atom-1">>}]},
           {<<"[\"~~hello\"]">>, [<<"~hello">>]},
           {<<"[\"~rhttp://google.com\"]">>, [transit_types:uri("http://google.com")]},
           {<<"[\"~u531a379e-31bb-4ce1-8690-158dceb64be6\"]">>, [transit_types:uuid("531a379e-31bb-4ce1-8690-158dceb64be6")]},
           {<<"[\"~#'\",\"~m0\"]">>, transit_types:datetime({0,0,0})},
           {<<"[\"~bc3VyZS4=\"]">>, [transit_types:binary(<<"sure.">>)]},
           {<<"[\"~#set\",[\"baz\",\"foo\",\"bar\"]]">>, sets:from_list([<<"foo">>, <<"bar">>, <<"baz">>])}
          ],
  [fun() -> Res = jsx:encode(transit_marshaler:marshal_top(?MODULE, Rep, {json, ?MODULE})) end || {Res, Rep} <- Tests].
-endif.
