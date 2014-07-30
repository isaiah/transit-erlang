-module(transit_json_marshaler).
-behaviour(gen_server).
-include_lib("transit_format.hrl").
-define(SERVER, ?MODULE).

-record(state, {started = queue:new(),
                is_key = queue:new()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/0, stop/0]).
-export([marshal_top/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  S = queue:from_list([true]),
  {ok, #state{started=S}}.

handle_call({marshal_top, Object}, _From, State=#state{}) ->
  Handler = transit_write_handlers:handler(Object),
  TagFun = Handler#write_handler.tag,
  Tag = TagFun(Object),
  {Ret, NewState} = if length(Tag) =:= 1 ->
                         marshal(#tagged_value{tag=?QUOTE, rep=Object}, State);
                       true ->
                         marshal(Object, State)
                    end,
  {reply, Ret, NewState};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
marshal_top(Obj) ->
  gen_server:call(?MODULE, {marshal_top, Obj}).

-spec marshal(as_map_key, {binary(), any()}, S) -> {bitstring(), S} when S :: #state{}.
marshal(as_map_key, Obj, S=#state{}) ->
  Handler = transit_write_handlers:handler(Obj),
  TagFun = Handler#write_handler.tag,
  StrRepFun = Handler#write_handler.string_rep,
  Rep = StrRepFun(Obj),
  case TagFun(Obj) of
    ?Null ->
      emit_null(as_map_key, Rep, S);
    ?Boolean ->
      emit_boolean(as_map_key, Rep, S);
    ?Int ->
      emit_int(as_map_key, Rep, S);
    ?String ->
      emit_string(as_map_key, <<"">>, Rep, S);
    T ->
      emit_encoded(T, Rep, S)
  end.

-spec marshal(any(), S) -> {bitstring(), S} when S :: #state{}.
marshal(TaggedVal=#tagged_value{}, S=#state{}) ->
  emit_tagged(TaggedVal, S);

marshal(Obj, S=#state{}) ->
  Handler = transit_write_handlers:handler(Obj),
  RepFun = Handler#write_handler.rep,
  Rep = RepFun(Obj),
  TagFun = Handler#write_handler.tag,
  case TagFun(Obj) of
    ?Null ->
      emit_object(undefined, S);
    ?Boolean ->
      emit_object(Rep, S);
    ?Int ->
      emit_object(Rep, S);
    ?Array ->
      emit_array(Rep, S);
    ?Map ->
      emit_map(Rep, S);
    ?String when is_bitstring(Rep) ->
      emit_string(<<"">>, Rep, S);
    ?String ->
      emit_string(<<"">>, list_to_binary(Rep), S);
    T when bit_size(T) =:= 8 ->
      emit_string(<<?ESC/bitstring, T/bitstring>>, list_to_binary(Rep), S);
    T ->
      emit_encoded(T, Rep, S)
  end.

emit_null(as_map_key, _Rep, S=#state{}) ->
  emit_string(as_map_key, <<?ESC/bitstring, ?Null/bitstring>>, <<"null">>, S).

emit_boolean(as_map_key, Rep, S) ->
  emit_string(as_map_key, <<?ESC/bitstring, ?Boolean/bitstring>>, Rep, S).

emit_int(as_map_key, Rep, S) ->
  emit_string(as_map_key, <<?ESC/bitstring, ?Int/bitstring>>, Rep, S).

emit_tagged(_TaggedValue=#tagged_value{tag=Tag, rep=Rep}, S=#state{}) ->
  {ArrayStart, S1} = emit_array_start(S),
  EncodedTag = transit_rolling_cache:encode(<<?ESC/bitstring, "#", Tag/bitstring>>),
  {Tag1, S2} = emit_object(EncodedTag, S1),
  {Body, S3} =  marshal(Rep, S2),
  {ArrayEnd, S4} = emit_array_end(S3),
  {<<ArrayStart/bitstring, Tag1/bitstring, Body/bitstring, ArrayEnd/bitstring>>, S4}.

emit_encoded(Tag, Rep, S) ->
  Handler = transit_write_handlers:handler(Rep),
  RepFun = Handler#write_handler.string_rep,
  StrRep = RepFun(Rep),
  emit_tagged(#tagged_value{tag=Tag, rep=StrRep}, S).

-spec emit_string(as_map_key, bitstring(), bitstring(), S) -> {bitstring(), S} when S :: #state{}.
emit_string(as_map_key, Tag, String, S=#state{}) ->
  Escaped = escape(String),
  Encoded = transit_rolling_cache:encode(as_map_key, <<Tag/bitstring, Escaped/bitstring>>),
  emit_object(Encoded, S).

-spec emit_string(bitstring(), bitstring(), S) -> {bitstring(), S} when S :: #state{}.
emit_string(Tag, String, S=#state{}) ->
  Escaped = escape(String),
  Encoded = transit_rolling_cache:encode(<<Tag/bitstring, Escaped/bitstring>>),
  emit_object(Encoded, S).

emit_object(Obj, S=#state{}) ->
  {Sep, S1} = write_sep(S),
  Body = if is_bitstring(Obj) ->
              quote_string(Obj);
            is_integer(Obj) ->
              list_to_binary(integer_to_list(Obj));
            is_float(Obj) ->
              list_to_binary(float_to_list(Obj));
            is_atom(Obj) ->
              case Obj of
                undefined ->
                  quote_string("null");
                _ ->
                  list_to_binary(atom_to_list(Obj))
              end;
            true ->
              case io_lib:printable_list(Obj) of
                true ->
                  quote_string(Obj);
                false ->
                  erlang:throws("don't know how to encode object.")
              end
         end,
  {<<Sep/bitstring, Body/bitstring>>, S1}.

%emit_map(M, S=#state{}) ->
%  {MapStart, S1} = emit_map_start(S),
%  {Body, S2} = maps:fold(fun (K, V, {In, NS1}) ->
%                        {MK, NS2} = marshal(as_map_key, K, NS1),
%                        {MV, NS3} = marshal(V, NS2),
%                        {<<In/bitstring, MK/bitstring, MV/bitstring>>, NS3}
%                    end,
%                    {<<"">>, S1}, M),
%  {MapEnd, S3} = emit_map_end(S2),
%  {<<MapStart/bitstring, Body/bitstring, MapEnd/bitstring>>, S3}.

emit_map(M, S) ->
  emit_array([?MAP_AS_ARR|flatten_map(M)], S).

emit_array(A, S=#state{}) ->
  {ArrayStart, S1} = emit_array_start(S),
  {Body, S2} = lists:foldl(fun (E, {In, NS1}) ->
                        {NE, NS2} = marshal(E, NS1),
                        {<<In/bitstring, NE/bitstring>>, NS2}
                    end,
                    {<<"">>, S1}, A),
  {ArrayEnd, S3} = emit_array_end(S2),
  {<<ArrayStart/bitstring, Body/bitstring, ArrayEnd/bitstring>>, S3}.

emit_array_start(S=#state{}) ->
  {Sep, S1} = write_sep(S),
  {<<Sep/bitstring, "[">>, push_level(S1)}.

emit_array_end(S=#state{}) ->
  S1 = pop_level(S),
  {<<"]">>, S1}.

emit_map_start(S=#state{}) ->
  {Sep, S1} = write_sep(S),
  {<<Sep/bitstring, "{">>, push_map(S1)}.

emit_map_end(S=#state{}) ->
  {<<"}">>, pop_level(S)}.

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

-spec is_escapable(string()) -> boolean().
is_escapable(S) ->
  case re:run(S, bitstring_to_list(<<"^\\", ?SUB/bitstring, "|", ?ESC/bitstring,"|",?RES/bitstring>>)) of
    {match, _} ->
      true;
    _ ->
      false
  end.

-spec push_level(S) -> S when S:: #state{}.
push_level(State=#state{started=S}) ->
  State#state{started=queue:in(true, S)}.

-spec pop_level(S) -> S when S:: #state{}.
pop_level(State=#state{started=S, is_key=K}) ->
  {_, S1} = queue:out_r(S),
  {_, K1} = queue:out_r(K),
  State#state{started=S1, is_key=K1}.

-spec push_map(S) -> S when S :: #state{}.
push_map(State=#state{started=S, is_key=K}) ->
  State#state{started=queue:in(true, S), is_key=queue:in(true, K)}.

-spec write_sep(S) -> {bitstring(), S} when S :: #state{}.
write_sep(State=#state{started=S, is_key=K}) ->
  case queue:out_r(S) of
    {{value, true}, S1} ->
      S2 = queue:in(false, S1),
      {<<"">>, State#state{started=S2}};
    _ ->
      case queue:out_r(K) of
        {{value, true}, K1} ->
          K2 = queue:in(false, K1),
          {<<":">>, State#state{is_key=K2}};
        {{value, false}, K1} ->
          K2 = queue:in(true, K1),
          {<<",">>, State#state{is_key=K2}};
        {empty, K} ->
          {<<",">>, State}
      end
  end.

quote_string(Str) ->
  EscapeSlash = re:replace(Str, "\\\\", "\\\\"),
  EscapeQuote = re:replace(EscapeSlash, "\\\"", "\\\""),
  <<"\"", EscapeQuote/bitstring, "\"">>.

flatten_map(M) ->
  maps:fold(fun(K, V, In) ->
                [K, V| In]
            end, [], M).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start_server() ->
  {ok, _} = start(),
  {ok, _} = transit_rolling_cache:start_link(),
  ok.

stop_server(ok) ->
  ok = transit_rolling_cache:stop(),
  ok = stop(),
  ok.

marshals_extention_test_() ->
  {foreach,
   fun start_server/0,
   fun stop_server/1,
   [fun marshals_tagged/1,
    fun marshals_tagged/1,
    fun marshals_array/1,
    fun marshals_map/1
   ]}.

marshals_tagged(ok) ->
  Started = queue:from_list([true]),
  Tests = [{<<"[\"~#'\",\"foo\"]">>, "foo"},
           {<<"[\"~#'\",\"foo\"]">>, <<"foo">>},
           {<<"[\"~#'\",1234]">>, 1234}],
  [fun() -> {Res, _} = emit_tagged(#tagged_value{tag=?QUOTE, rep=Rep}, #state{started=Started}) end || {Res, Rep} <- Tests].

marshals_array(ok) ->
  ?_assertEqual(<<"[\"a\",2,\"~:a\"]">>, marshal_top(["a",2, a])).

marshals_map(ok) ->
  ?_assertEqual(<<"[\"^ \",\"~:a\",\"~:b\",3,4]">>, marshal_top(#{a => b, 3 => 4})).

-endif.
