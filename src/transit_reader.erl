-module(transit_reader).
-behaviour(gen_server).
-include_lib("transit_format.hrl").

-define(SERVER, ?MODULE).

-record(state, {env::transit_marshaler:env(),
                unmarshaler=transit_json_unmarshaler}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, read/1]).

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

read(Name) ->
  gen_server:call(?MODULE, {read, jsx:decode(Name)}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  transit_rolling_cache:start_link(),
  {ok, false}.

handle_call({read, Name}, _From, AsMapKey) ->
  Val = decode(Name, false),
  {reply, Val, AsMapKey};
handle_call(_Request, _From, AsMapKey) ->
  {reply, ok, AsMapKey}.

handle_cast(_Msg, AsMapKey) ->
  {noreply, AsMapKey}.

handle_info(_Info, AsMapKey) ->
  {noreply, AsMapKey}.

terminate(_Reason, _AsMapKey) ->
  ok.

code_change(_OldVsn, AsMapKey, _Extra) ->
  {ok, AsMapKey}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
decode(Name, AsMapKey) when is_bitstring(Name) ->
  decode_string(Name, AsMapKey);
decode(Name, AsMapKey) when is_list(Name) ->
  case Name of
    [?MAP_AS_ARR|Tail] ->
      decode_array_hash(Tail, AsMapKey);
    [{_, _}|_] -> 
      decode_hash(Name, AsMapKey);
    [EscapedTag, Rep] ->
      <<"~", "#", Tag/binary>> = EscapedTag,
      decode_tag(Tag, Rep, AsMapKey);
    _ ->
      decode_array(Name, AsMapKey)
  end;
decode(Name, _AsMapKey) ->
  Name.

decode_string(String, AsMapKey) ->
  parse_string(transit_rolling_cache:decode(String, AsMapKey), AsMapKey).

parse_string(String, _AsMapKey) ->
  case String of
    [?ESC,Tag|Rep] ->
      case transit_read_handlers:handler(Tag) of
        F when is_function(F) ->
          F(Rep);
        _ ->
          if Tag =:= ?ESC; Tag =:= ?SUB; Tag =:= ?RES ->
               [Tag|Rep];
             Tag =:= "#" ->
               Rep;
             true ->
               #tagged_value{tag=Tag, rep=Rep}
          end
      end;
    _ ->
      String
  end.

decode_array_hash(Name, AsMapKey) ->
  [{decode(Key, true), decode(Val, AsMapKey)} || {Key, Val} <- Name].

decode_array(Name, AsMapKey) ->
  [decode(El, AsMapKey) || El <- Name].

decode_tag(Tag, Rep, _AsMapKey) ->
  F = transit_read_handlers:handler(Tag),
  F(Rep).

decode_hash(Name, AsMapKey) when length(Name) =:= 1 ->
  [{decode(Key, AsMapKey), decode(Val, AsMapKey)} || {Key, Val} <- Name];
decode_hash(Name, AsMapKey) ->
  case Name of
    {[?ESC, "#"|Tag], Val} ->
      decode_tag(Tag, Val, AsMapKey);
    {Key, Val} ->
      {Rep, _} = decode(Val, AsMapKey),
      {Key, Rep};
    _ ->
      erlang:throw({"unkown hash format: ", Name})
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start() ->
  transit_rolling_cache:start_link(),
  ok.

stop(ok) ->
  ok = transit_rolling_cache:stop(),
  ok.

unmarshal_test_() ->
  {foreach,
   fun start/0,
   fun stop/1,
   [fun unmarshal_quoted/1]}.

unmarshal_quoted(ok) ->
  Env = transit_marshaler:new_env(),
  Tests = [{1, <<"[\"~#'\", 1]">>},
           {<<"foo">>, <<"[\"~#'\", \"foo\"]">>},
           {undefined, <<"[\"~#'\", null]">>}
          ],
  [fun() -> Val = decode(jsx:decode(Str), false) end || {Val, Str} <- Tests].
-endif.
