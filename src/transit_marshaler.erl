-module(transit_marshaler).
-include_lib("transit_format.hrl").

-export([flatten_map/1, quote_string/1, escape/1, as_map_key/1, force_as_map_key/2]).
-export([marshal_top/3, marshal/3, new_env/0, new_env/1, cache/1]).

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

-callback emit_cmap(Rep, Env) ->
  {Rep, Env}
    when Rep::term(), Env::env().

-callback emit_string(Tag, Rep, Env) ->
  {Rep, Env}
    when Tag::bitstring(), Rep::bitstring(), Env::env().

-callback handler(Obj) ->
  Handler when Obj::term(), Handler::transit_write_handlers:writer_handler().

%%% flatten R17 map
flatten_map(M) when is_map(M) ->
  maps:fold(fun(K, V, In) ->  [K, V| In] end, [], M);
flatten_map([{}]) -> [];
flatten_map([{K,V}|Tail]) -> [K,V|flatten_map(Tail)];
flatten_map([]) -> [].

quote_string(Str) ->
  EscapeSlash = re:replace(Str, "\\\\", "\\\\"),
  EscapeQuote = re:replace(EscapeSlash, "\\\"", "\\\""),
  <<"\"", EscapeQuote/bitstring, "\"">>.

-spec escape(binary()) -> binary().
escape(?MAP_AS_ARR) -> ?MAP_AS_ARR;
escape(S) ->
    case re:run(S, binary_to_list(<<"^\\", ?SUB/binary, "|", ?ESC/binary,"|",?RES/binary>>)) of
        {match, _} -> <<?ESC/binary, S/binary>>;
        _ -> S
    end.

-spec as_map_key(env()) -> boolean().
as_map_key(#env{ as_map_key = K }) -> K.

-spec cache(env()) -> pid().
cache( #env{ cache = C }) -> C.

force_as_map_key(AsMapKey, Env) ->
  Env#env{as_map_key=AsMapKey}.

find_handler(Mod, Obj, Env) ->
  case Mod:handler(Obj) of
    undefined ->
      find_handler_default_handlers(Obj, Env);
    Handler -> Handler
  end.

find_handler_default_handlers(Obj, #env { custom_handler = CHandler }) ->
  case transit_write_handlers:handler(Obj) of
    undefined -> CHandler:handler(Obj);
    Handler -> Handler
  end.

marshal_top(Mod, Object, Conf) ->
  Env = new_env(Conf),
  #write_handler { tag = TagFun } = find_handler(Mod, Object, Env),
  {Ret, _Env} = marshal_top_output(Mod, Object, Env, TagFun(Object)),
  Ret.
  
marshal_top_output(Mod, Object, Env, Tag) when byte_size(Tag) == 1 ->
  Mod:emit_tagged(#tagged_value { tag = ?QUOTE, rep = Object}, Env);
marshal_top_output(Mod, Object, Env, _) ->
  marshal(Mod, Object, Env).

-spec marshal(module(), any(), S) -> {bitstring(), S} when S :: env().

marshal(Mod, Obj, #env { as_map_key = AsMapKey } = S) ->
  #write_handler { tag = TagFun,
                   string_rep = StringRep,
                   rep = Repr } = find_handler(Mod, Obj, S),
  Rep = case AsMapKey of
          true -> StringRep(Obj);
          false -> Repr(Obj)
        end,
  case emit_ground(Mod, Rep, S, TagFun(Obj)) of
    {extension, Tag} when is_list(Rep), byte_size(Tag) == 1 ->
      Bin = list_to_binary(Rep),
      Mod:emit_string(<<?ESC/binary, Tag/binary>>, Bin, S);
    {extension, Tag} when byte_size(Tag) == 1 ->
      Mod:emit_string(<<?ESC/binary, Tag/binary>>, StringRep(Obj), S);
    {extension, Tag} ->
      Mod:emit_encoded(Tag, Rep, S);
    V -> V
  end.
  
emit_ground(Mod, Rep, S, ?Null) -> Mod:emit_null(Rep, S);
emit_ground(Mod, Rep, S, ?Boolean) -> Mod:emit_boolean(Rep, S);
emit_ground(Mod, Rep, S, ?Int) -> Mod:emit_int(Rep, S);
emit_ground(Mod, Rep, S, ?Float) -> Mod:emit_float(Rep, S);
emit_ground(Mod, Rep, S, ?Array) -> Mod:emit_array(Rep, S);
emit_ground(Mod, Rep, S, ?String) when is_binary(Rep) -> Mod:emit_string(<<>>, Rep, S);
emit_ground(Mod, Rep, S, ?String) -> Mod:emit_string(<<>>, list_to_binary(Rep), S);
emit_ground(Mod, Rep, S, ?Map) ->
  case stringable_keys(Rep) of
    true -> Mod:emit_map(Rep, S);
    false -> Mod:emit_cmap(Rep, S)
  end;
emit_ground(_Mod, _Rep, _S, T) -> {extension, T}.

new_env() ->
  #env{cache=transit_rolling_cache:empty(json)}.

-spec new_env({atom(), module()}) -> env().
new_env({Format, CustomHandler}) ->
  Env = new_env(),
  Cache = transit_rolling_cache:empty(Format),
  Env#env{custom_handler=CustomHandler, cache=Cache}.

stringable_keys(Rep) when is_map(Rep) ->
  lists:all(fun(X) ->
                bit_size(transit_write_handlers:tag(X)) =:= 8
            end, maps:keys(Rep));
stringable_keys([{}]) ->
  true;
stringable_keys([]) ->
  true;
stringable_keys([{K, _}|T]) ->
  case bit_size(transit_write_handlers:tag(K)) =:= 8 of
    true ->
      stringable_keys(T);
    false ->
      false
  end.
