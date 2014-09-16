-module(transit_json_verbose_marshaler).
-behaviour(transit_marshaler).
-include("transit_format.hrl").
-include("transit_types.hrl").

-export([emit_null/2, emit_boolean/2, emit_int/2, emit_float/2]).
-export([emit_string/3, emit_object/2, emit_tagged/2]).
-export([emit_encoded/3, emit_array/2, emit_map/2]).
-export([emit_cmap/2, handler/1]).

-spec emit_map(Rep, Env) ->
  {Resp, Env} when Rep::term(), Resp::bitstring(), Env::transit_marshaler:env().
emit_map(M, S1) when is_map(M) ->
  {Body, S2} = maps:fold(fun (K, V, {In, NS1}) ->
                        {MK, NS2} = transit_marshaler:marshal(?MODULE, K, transit_marshaler:force_as_map_key(true, NS1)),
                        {MV, NS3} = transit_marshaler:marshal(?MODULE, V, transit_marshaler:force_as_map_key(false, NS2)),
                        {[{MK, MV}|In], NS3}
                    end,
                    {[], S1}, M),
  case Body of
    [] ->
      {[{}], S2}; % handle of empty map
    _ ->
      {Body, S2}
  end;
emit_map([], Env) ->
  {[], Env};
emit_map([{}], Env) ->
  {[{}], Env};
emit_map([{K,V}|Tail], Env) ->
  {MK, NS1} = transit_marshaler:marshal(?MODULE, K, transit_marshaler:force_as_map_key(true, Env)),
  {MV, NS2} = transit_marshaler:marshal(?MODULE, V, transit_marshaler:force_as_map_key(false, NS1)),
  {MTail, NS3} = emit_map(Tail, NS2),
  {[{MK, MV}|MTail], NS3}.

emit_cmap(M, Env) ->
  transit_json_marshaler:emit_cmap(M, Env).

emit_string(<<>>, Rep, Env) ->
  Escaped = transit_marshaler:escape(Rep),
  emit_object(Escaped, Env);
emit_string(Tag, Rep, Env) ->
  emit_object(<<Tag/binary, Rep/binary>>, Env).

emit_tagged(_TaggedValue=#tagged_value{tag=Tag, rep=Rep}, S) ->
  S0 = transit_marshaler:force_as_map_key(true, S),
  Cache = transit_marshaler:cache(S),
  {EncodedTag, Cache1} = transit_rolling_cache:encode(Cache, <<?ESC/bitstring, "#", Tag/bitstring>>, transit_marshaler:as_map_key(S0)),
  S1 = S0#env{cache=Cache1},
  {Tag1, S2} = emit_object(EncodedTag, S1),
  S3 = transit_marshaler:force_as_map_key(false, S2),
  {Body, S4} =  transit_marshaler:marshal(?MODULE, Rep, S3),
  {[{Tag1, Body}], S4}.

%%% delegate to transit_json_marshaler
emit_object(Rep, Env) ->
  transit_json_marshaler:emit_object(Rep, Env).

emit_array(Rep, Env) ->
  transit_json_marshaler:emit_array(Rep, Env).

emit_boolean(Rep, Env) ->
  transit_json_marshaler:emit_boolean(Rep, Env).

emit_int(Rep, Env) ->
  transit_json_marshaler:emit_int(Rep, Env).

emit_float(Rep, Env) ->
  transit_json_marshaler:emit_float(Rep, Env).

emit_encoded(Tag, Rep, Env) ->
  transit_json_marshaler:emit_encoded(Tag, Rep, Env).

emit_null(Rep, Env) ->
  transit_json_marshaler:emit_null(Rep, Env).

handler({timepoint, _}) ->
  #write_handler {
	tag = fun(_) -> ?VERBOSEDATE end,
	rep = fun({timepoint, TP}) -> transit_utils:iso_8601_fmt(TP) end,
	string_rep = fun({timepoint, TP}) -> transit_utils:iso_8601_fmt(TP) end
  };
handler(_) -> undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
new_env() ->
  transit_marshaler:new_env().

stop(_Env) ->
  ok.

marshals_extention_test_() ->
  {foreach,
   fun new_env/0,
   fun stop/1,
   [fun marshals_tagged/1,
    fun marshals_extend/1
   ]}.

marshals_tagged(Env) ->
  Tests = [{<<"{\"~#'\":[102,111,111]}">>, "foo"},
           {<<"{\"~#'\":\"foo\"}">>, <<"foo">>},
           {<<"{\"~#'\":1234}">>, 1234}],
  [fun() -> {Raw, _} = emit_tagged(#tagged_value{tag=?QUOTE, rep=Rep}, Env),
            Res = jsx:encode(Raw)
   end || {Res, Rep} <- Tests].

marshals_extend(_Env) ->
  Tests = [{<<"{}">>, [{}]},
           {<<"{}">>, #{}},
           {<<"[\"a\",2,\"~:a\"]">>, [<<"a">>, 2, {kw, <<"a">>}]},
           {<<"{\"a\":\"b\",\"~i3\":4}">>, #{3 => 4, <<"a">> => <<"b">>}},
           {<<"{\"~#'\":\"~t1970-01-01T00:00:00.000Z\"}">>, {timepoint, {0,0,0}}},
           {<<"{\"~d3.5\":4.1}">>, #{3.5 => 4.1}}],
  [fun() -> Res = jsx:encode(transit_marshaler:marshal_top(?MODULE, Rep, {json_verbose, ?MODULE})) end || {Res, Rep} <- Tests].
-endif.
