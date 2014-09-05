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

find_handler(Obj, M, Env) ->
  case M:handler(Obj) of
    undefined ->
      find_handler_default_handlers(Obj, Env);
    Handler -> Handler
  end.

find_handler_default_handlers(Obj, #env { custom_handler = CHandler }) ->
  case transit_write_handlers:handler(Obj) of
    undefined -> CHandler:handler(Obj);
    Handler -> Handler
  end.

marshal_top(M, Object, Conf) ->
  Env = new_env(Conf),
  #write_handler { tag = TagFun } = find_handler(Object, M, Env),
  {Ret, _Env} = marshal_top_output(M, Object, Env, TagFun(Object)),
  Ret.
  
marshal_top_output(M, Object, Env, Tag) when bit_size(Tag) == 8 ->
  M:emit_tagged(#tagged_value { tag = ?QUOTE, rep = Object}, Env);
marshal_top_output(M, Object, Env, _) ->
  marshal(M, Object, Env).

-spec marshal(module(), any(), S) -> {bitstring(), S} when S :: env().
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
      case stringable_keys(Rep) of
        true ->
          Name:emit_map(Rep, S);
        false ->
          Name:emit_cmap(Rep, S)
      end;
    ?String when is_bitstring(Rep) ->
      Name:emit_string(<<>>, Rep, S);
    ?String ->
      Name:emit_string(<<>>, list_to_binary(Rep), S);
    T when bit_size(T) =:= 8 ->
      BinaryRep = case is_list(Rep) of
                    true ->
                      list_to_binary(Rep);
                    false ->
                      F = Handler#write_handler.string_rep,
                      F(Obj)
                  end,
      Name:emit_string(<<?ESC/bitstring, T/bitstring>>, BinaryRep, S);
    T ->
      Name:emit_encoded(T, Rep, S)

  end.

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
