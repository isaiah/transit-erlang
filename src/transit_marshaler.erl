-module(transit_marshaler).
-include_lib("transit_format.hrl").
-record(env, {started = queue:new() :: queue:queue(boolean()),
              is_key = queue:new() :: queue:queue(boolean()),
              as_map_key=false :: boolean(),
              custom_handler :: module()
             }).

-type env()::#env{}.
-export_type([env/0]).

-export([write_sep/1, emit_array_start/1, emit_array_end/1, emit_map_start/1, emit_map_end/1]).
-export([flatten_map/1, quote_string/1, escape/1, is_escapable/1, as_map_key/1, force_as_map_key/2]).
-export([marshal_top/3, marshal/3, new_env/0, new_env/1]).

-callback emit_null(Rep, Env) ->
  {Rep, Env}
    when Rep::bitstring(), Env::env().

-callback emit_boolean(Rep, Env) ->
  {Rep, Env}
    when Rep::bitstring(), Env::env().

-callback emit_int(Rep, Env) ->
  {Rep, Env}
    when Rep::bitstring(), Env::env().

-callback emit_float(Rep, Env) ->
  {Rep, Env}
    when Rep::bitstring(), Env::env().

-callback emit_tagged(Rep, Env) ->
  {Resp, Env}
    when Rep::tagged_value(), Resp::bitstring(), Env::env().

-callback emit_object(Rep, Env) ->
  {Resp, Env}
    when Rep::term(), Resp::bitstring(), Env::env().

-callback emit_encoded(Tag, Rep, Env) ->
  {Rep, Env}
    when Tag::bitstring(), Rep::bitstring(), Env::env().

-callback emit_array(Rep, Env) ->
  {Resp, Env}
    when Rep::term(), Resp::bitstring(), Env::env().

-callback emit_map(Rep, Env) ->
  {Rep, Env}
    when Rep::term(), Env::env().

-callback emit_string(Tag, Rep, Env) ->
  {Rep, Env}
    when Tag::bitstring(), Rep::bitstring(), Env::env().

-callback handler(Obj) ->
  Handler when Obj::term(), Handler::transit_writer_handlers:writer_handler().

%%% flatten R17 map
flatten_map(M) when is_map(M) ->
  maps:fold(fun(K, V, In) ->
                [K, V| In]
            end, [], M);
%%% flatten proplists style map
flatten_map([{K,V}|Tail]) ->
  [K,V|flatten_map(Tail)];
flatten_map([]) ->
  [].

-spec emit_array_start(Env) ->
  {ArrayStart, Env} when ArrayStart::bitstring(), Env::env().
emit_array_start(S) ->
  {Sep, S1} = write_sep(S),
  {<<Sep/bitstring, "[">>, push_level(S1)}.

-spec emit_array_end(Env) ->
  {ArrayEnd, Env} when ArrayEnd::bitstring(), Env::env().
emit_array_end(S) ->
  S1 = pop_level(S),
  {<<"]">>, S1}.

-spec emit_map_start(Env) ->
  {MapStart, Env} when MapStart::bitstring(), Env::env().
emit_map_start(S) ->
  {Sep, S1} = write_sep(S),
  {<<Sep/bitstring, "{">>, push_map(S1)}.

-spec emit_map_end(Env) ->
  {MapEnd, Env} when MapEnd::bitstring(), Env::env().
emit_map_end(S) ->
  {<<"}">>, pop_level(S)}.

-spec push_level(S) -> S when S::env().
push_level(Env=#env{started=S}) ->
  Env#env{started=queue:in(true, S)}.

-spec pop_level(S) -> S when S:: env().
pop_level(Env=#env{started=S, is_key=K}) ->
  {_, S1} = queue:out_r(S),
  {_, K1} = queue:out_r(K),
  Env#env{started=S1, is_key=K1}.

-spec push_map(S) -> S when S::env().
push_map(State=#env{started=S, is_key=K}) ->
  State#env{started=queue:in(true, S), is_key=queue:in(true, K)}.

-spec write_sep(S) -> {bitstring(), S} when S::env().
write_sep(Env=#env{started=S, is_key=K}) ->
  case queue:out_r(S) of
    {{value, true}, S1} ->
      S2 = queue:in(false, S1),
      {<<"">>, Env#env{started=S2}};
    _ ->
      case queue:out_r(K) of
        {{value, true}, K1} ->
          K2 = queue:in(false, K1),
          {<<":">>, Env#env{is_key=K2}};
        {{value, false}, K1} ->
          K2 = queue:in(true, K1),
          {<<",">>, Env#env{is_key=K2}};
        {empty, K} ->
          {<<",">>, Env}
      end
  end.

quote_string(Str) ->
  EscapeSlash = re:replace(Str, "\\\\", "\\\\"),
  EscapeQuote = re:replace(EscapeSlash, "\\\"", "\\\""),
  <<"\"", EscapeQuote/bitstring, "\"">>.

-spec escape(bitstring()) -> bitstring().
escape(S) ->
  if S =:= ?MAP_AS_ARR ->
       S;
     true ->
       case is_escapable(S) of
         true ->
           <<?ESC/bitstring,S/bitstring>>;
         false ->
           S
       end
  end.

-spec is_escapable(bitstring()) -> boolean().
is_escapable(S) ->
  case re:run(S, bitstring_to_list(<<"^\\", ?SUB/bitstring, "|", ?ESC/bitstring,"|",?RES/bitstring>>)) of
    {match, _} ->
      true;
    _ ->
      false
  end.

-spec as_map_key(env()) -> boolean().
as_map_key(Env) ->
  Env#env.as_map_key.

force_as_map_key(AsMapKey, Env) ->
  Env#env{as_map_key=AsMapKey}.

find_handler(Obj, M, Env) ->
  case M:handler(Obj) of
    undefined ->
      case transit_write_handlers:handler(Obj) of
        undefined ->
          CustomHandler = Env#env.custom_handler,
          CustomHandler:handler(Obj);
        Handler ->
          Handler
      end;
    Handler ->
      Handler
  end.

marshal_top(M, Object, Env) ->
  Handler = find_handler(Object, M, Env),
  TagFun = Handler#write_handler.tag,
  Tag = TagFun(Object),
  if bit_size(Tag) =:= 8 ->
       marshal(M, #tagged_value{tag=?QUOTE, rep=Object}, Env);
     true ->
       marshal(M, Object, Env)
  end.

-spec marshal(module(), any(), S) -> {bitstring(), S} when S :: env().
marshal(Name, TaggedVal=#tagged_value{}, Env) ->
  Name:emit_tagged(TaggedVal, Env);

marshal(Name, Obj, S) ->
  Handler = find_handler(Obj, Name, S),
  TagFun = Handler#write_handler.tag,
  RepFun = case S#env.as_map_key of
                true ->
                  Handler#write_handler.string_rep;
                false ->
                  Handler#write_handler.rep
              end,

  Rep = RepFun(Obj),
  case TagFun(Obj) of
    ?Null ->
      Name:emit_null(Rep, S);
    ?Boolean ->
      Name:emit_boolean(Rep, S);
    ?Int ->
      Name:emit_int(Rep, S);
    ?Float ->
      Name:emit_float(Rep, S);
    ?Array ->
      Name:emit_array(Rep, S);
    ?Map ->
      Name:emit_map(Rep, S);
    ?String when is_bitstring(Rep) ->
      Name:emit_string(<<>>, Rep, S);
    ?String ->
      Name:emit_string(<<>>, list_to_binary(Rep), S);
    T when bit_size(T) =:= 8 ->
      case is_list(Rep) of
        true ->
          Name:emit_string(<<?ESC/bitstring, T/bitstring>>, list_to_binary(Rep), S);
        false ->
          F = Handler#write_handler.string_rep,
          StrRep = F(Obj),
          Name:emit_string(<<?ESC/bitstring, T/bitstring>>, StrRep, S)
      end;
    T ->
      Name:emit_encoded(T, Rep, S)

  end.

new_env() ->
  S = queue:from_list([true]),
  #env{started=S}.

-spec new_env(module()) -> env().
new_env(CustomHandler) ->
  S = queue:from_list([true]),
  #env{started=S, custom_handler=CustomHandler}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_escapeable_test() ->
  ?assertNot(is_escapable(<<"foo">>)),
  ?assert(is_escapable(<<"^ foo">>)).
-endif.
