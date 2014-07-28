-module(transit_json_marshaler).
-behaviour(gen_server).
-include_lib("transit_format.hrl").
-define(SERVER, ?MODULE).

-record(state, {started = queue:new(),
                is_key = queue:new()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
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
  Tag = apply(Handler#write_handler.tag, [Object]),
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

marshal(as_map_key, {?Null, _}, _S=#state{}) ->
  emit_string(as_map_key, ?ESC, ?Null, undefined);

marshal(as_map_key, {?Boolean, Val}, _S=#state{}) ->
  emit_string(as_map_key, ?ESC, ?Boolean, Val).


marshal({?Null, _Null}, _S=#state{}) ->
  emit_object(undefined);
marshal({?Boolean, Val}, _S=#state{}) ->
  emit_object(Val);
marshal({?Int, I}, _S=#state{}) ->
  emit_object(I);
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
  emit_encoded(Tag, Rep, S).

emit_tagged(Tag, Rep, S=#state{}) ->
  {ArrayStart, S1} = emit_array_start(S),
  EncodedTag = transit_rolling_cache:encode(?ESC ++ Tag),
  {Body, S2} =  marshal(Rep, S1),
  {ArrayEnd, S3} = emit_array_end(S2),
  {ArrayStart ++ emit_object(EncodedTag) ++ Body ++ ArrayEnd, S3}.

emit_encoded(Tag, Rep, S) when length(Tag) =:= 1 ->
  case io_lib:printable_list(Rep) of
    true ->
      emit_string(?ESC ++ Tag, Rep, S);
    false ->
      erlang:throw(io_lib:format("Cannot be encoded as string: ~s", [Rep]))
  end;
emit_encoded(Tag, Rep, S) ->
  emit_tagged(Tag, Rep, S).

emit_string(as_map_key, Prefix, Tag, String) ->
  Encoded = transit_rolling_cache:encode(as_map_key, Prefix ++ Tag ++ escape(String)),
  emit_object(Encoded).

emit_string(Prefix, Tag, String) ->
  {ok, Encoded} = transit_rolling_cache:encode(Prefix ++ Tag ++ escape(String)),
  emit_object(Encoded).

emit_object(Obj) ->
  if is_bitstring(Obj) ->
       EscapeSlash = re:replace(Obj, "\\\\", "\\\\"),
       EscapeQuote = re:replace(EscapeSlash, "\\\"", "\\\""),
       "\\\"" ++ EscapeQuote ++ "\\\"";
     is_number(Obj) ->
       io_lib:format("~p", [Obj]);
     is_atom(Obj) ->
       atom_to_list(Obj)
  end.

emit_array_start(S=#state{}) ->
  {Sep, S1} = write_sep(S),
  S2 = push_level(S1),
  {Sep ++ "[", S2}.

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

escape(S) ->
  if S =:= ?MAP_AS_ARR ->
       S;
     true ->
       case is_escapable(S) of
         true ->
           ?ESC ++ S;
         false ->
           S
       end
  end.

is_escapable(S) ->
  case re:run(S, "^\\" ++ ?SUB ++ "|" ++ ?ESC ++ "|" ++ ?RES) of
    {match, _} ->
      true;
    _ ->
      false
  end.

push_level(State=#state{started=S, is_key=K}) ->
  State#state{started=queue:in(true, S), is_key=queue:in(true, K)}.

pop_level(State=#state{started=S1, is_key=K1}) ->
  S2 = queue:tail(S1),
  K2 = queue:tail(K1),
  State#state{started=S2, is_key=K2}.

write_sep(State=#state{started=S, is_key=K}) ->
  case queue:peek(S) of
    {value, true} ->
      queue:tail(S),
      S1 = queue:in(false, S),
      {"", State#state{started=S1}};
    _ ->
      case queue:peek(K) of
        true ->
          queue:tail(K),
          K1 = queue:in(false, K),

          {":", State#state{started=K1}};
        false ->
          queue:tail(K),
          K1 = queue:in(true, K),
          {",", State#state{started=K1}};
        empty ->
          {",", State}
      end
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start() ->
  {ok, Pid} = ?MODULE:start_link(),
  Pid.

emit_string_test() ->
  {"~#s: foo", S} = marshal_top("foo").

-endif.
