-module(transit_json_marshaler).
-behaviour(gen_server).
-include_lib("transit_format.hrl").
-define(SERVER, ?MODULE).

-record(state, {started = queue:new(),
                is_key = queue:new()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/0]).
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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  S = queue:from_list([true]),
  {ok, #state{started=S}}.

handle_call({emit_object, Object, _AsMapKey}, _From, State) ->
  Rep = if is_bitstring(Object) ->
             <<"\"">> ++ Object ++ <<"\"">>;
           is_integer(Object) ->
             integer_to_list(Object);
           is_float(Object) ->
             float_to_list(Object);
           is_atom(Object) ->
             atom_to_list(Object)
        end,
  {reply, Rep, State};

handle_call({marshal_top, Object}, _From, State=#state{}) ->
  Handler = transit_write_handlers:handler(Object),
  TagFun = Handler#write_handler.tag,
  Tag = TagFun(Object),
  {Ret, NewState} = if length(Tag) =:= 1 ->
                         marshal({?QUOTE, Object}, State);
                       true ->
                         marshal({Tag, Object}, State)
                    end,
  {reply, Ret, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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

-spec marshal(as_map_key, {binary(), any()}, S) -> {string(), S} when S :: #state{}.
marshal(as_map_key, {?Null, _}, _S=#state{}) ->
  emit_string(as_map_key, ?ESC, ?Null, "null");

marshal(as_map_key, {?Boolean, Val}, _S=#state{}) ->
  emit_string(as_map_key, ?ESC, ?Boolean, Val).


-spec marshal({string(), any()}, S) -> {string(), S} when S :: #state{}.
marshal({?Null, _Null}, S=#state{}) ->
  emit_object(undefined, S);
marshal({?Boolean, Val}, S=#state{}) ->
  emit_object(Val, S);
marshal({?Int, I}, S=#state{}) ->
  emit_object(I, S);
marshal({?Map, M}, S=#state{}) ->
  {MapStart, S1} = emit_map_start(S),
  {Body, S2} = maps:foldl(fun (K, V, {In, NS1}) ->
                        {MK, NS2} = marshal(as_map_key, K, NS1),
                        {MV, NS3} = marshal(V, NS2),
                        {In ++ MK ++ MV, NS3}
                    end,
                    {"", S1}, M),
  {MapEnd, S3} = emit_map_end(S2),
  {MapStart ++ Body ++ MapEnd, S3};
marshal({?QUOTE, Rep}, S=#state{}) ->
  emit_tagged(?QUOTE, Rep, S);
marshal({Tag, Rep}, S=#state{}) ->
  emit_encoded(?ESC, Tag, Rep, S);
marshal(Rep, S) ->
  Handler = transit_write_handlers:handler(Rep),
  TagFun = Handler#write_handler.tag,
  Tag = TagFun(Rep),
  emit_encoded("", Tag, Rep, S).

emit_tagged(Tag, Rep, S=#state{}) ->
  {ArrayStart, S1} = emit_array_start(S),
  EncodedTag = transit_rolling_cache:encode(?ESC ++ "#" ++ Tag),
  {Tag1, S2} = emit_object(EncodedTag, S1),
  {Body, S3} =  marshal(Rep, S2),
  {ArrayEnd, S4} = emit_array_end(S3),
  {ArrayStart ++ Tag1 ++ Body ++ ArrayEnd, S4}.

emit_encoded(_Prefix, Tag, Rep, S) when length(Tag) =:= 1 ->
  case io_lib:printable_list(Rep) of
    true ->
      emit_string("", Rep, S);
    false ->
      erlang:throw(io_lib:format("Cannot be encoded as string: ~s", [Rep]))
  end;
emit_encoded(Prefix, Tag, Rep, S) ->
  emit_tagged(Prefix ++ Tag, Rep, S).

-spec emit_string(as_map_key, string(), string(), S) -> {string(), S} when S :: #state{}.
emit_string(as_map_key, Tag, String, S=#state{}) ->
  Encoded = transit_rolling_cache:encode(as_map_key, Tag ++ escape(String)),
  emit_object(Encoded, S).

-spec emit_string(string(), string(), string()) -> string().
emit_string(Tag, String, S=#state{}) ->
  Encoded = transit_rolling_cache:encode(Tag ++ escape(String)),
  emit_object(Encoded, S).

emit_object(Obj, S=#state{}) ->
  {Sep, S1} = write_sep(S),
  Body = if is_bitstring(Obj) ->
              quote_string(Obj);
            is_number(Obj) ->
              io_lib:format("~p", [Obj]);
            is_atom(Obj) ->
              atom_to_list(Obj);
            true ->
              case io_lib:printable_list(Obj) of
                true ->
                  quote_string(Obj);
                false ->
                  erlang:throws("don't know how to encode object.")
              end
         end,
  {Sep ++ Body, S1}.

emit_array_start(S=#state{}) ->
  {Sep, S1} = write_sep(S),
  {Sep ++ "[", push_level(S1)}.

emit_array_end(S=#state{}) ->
  S1 = pop_level(S),
  {"]", S1}.

emit_map_start(S=#state{}) ->
  {Sep, S1} = write_sep(S),
  S1 = push_level(S),
  {Sep ++ "{", push_level(S1)}.

emit_map_end(S=#state{}) ->
  S1 = pop_level(S),
  {"}", S1}.

-spec escape(string()) -> string().
escape(S) when S =:= ?MAP_AS_ARR ->
  S;
escape(S) ->
  case is_escapable(S) of
    true ->
      ?ESC ++ S;
    false ->
      S
  end.

-spec is_escapable(string()) -> boolean().
is_escapable(S) ->
  case re:run(S, "^\\" ++ ?SUB ++ "|" ++ ?ESC ++ "|" ++ ?RES) of
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

-spec write_sep(S) -> {string(), S} when S :: #state{}.
write_sep(State=#state{started=S, is_key=K}) ->
  case queue:out_r(S) of
    {{value, true}, S1} ->
      S2 = queue:in(false, S1),
      {"", State#state{started=S2}};
    _ ->
      case queue:out_r(K) of
        {{value, true}, K1} ->
          K2 = queue:in(false, K1),
          {":", State#state{is_key=K2}};
        {{value, false}, K1} ->
          K2 = queue:in(true, K1),
          {",", State#state{is_key=K2}};
        {empty, K} ->
          {",", State}
      end
  end.

quote_string(Str) ->
  EscapeSlash = re:replace(Str, "\\\\", "\\\\"),
  EscapeQuote = re:replace(EscapeSlash, "\\\"", "\\\""),
  "\"" ++ EscapeQuote ++ "\"".


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start_server() ->
  {ok, _} = start(),
  {ok, _} = transit_rolling_cache:start_link(),
  ok.

emit_string_test_() ->
  {setup,
   fun start_server/0,
   fun marshal_string/1}.

marshal_string(ok) ->
  [?_assertEqual("[\"~#'\",\"foo\"]", marshal_top("foo"))].

-endif.
