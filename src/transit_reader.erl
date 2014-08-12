-module(transit_reader).
-behaviour(gen_server).
-include_lib("transit_format.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/0, read/1]).

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
      decode_tag(Tag, decode(Rep, AsMapKey), AsMapKey);
    _ ->
      decode_array(Name, AsMapKey)
  end;
decode(Name, _AsMapKey) ->
  Name.

decode_string(String, AsMapKey) ->
  parse_string(transit_rolling_cache:decode(String, AsMapKey), AsMapKey).

parse_string(String, _AsMapKey) ->
  case String of
    <<"~",Tag:8/binary-unit:1, Rep/binary>> ->
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

decode_array_hash([Key,Val|Name], AsMapKey) ->
  [{decode(Key, true), decode(Val, AsMapKey)}|decode_array_hash(Name, AsMapKey)];
decode_array_hash([], _AsMapKey) ->
  [].

decode_array(Name, AsMapKey) ->
  [decode(El, AsMapKey) || El <- Name].

decode_tag(Tag, Rep, _AsMapKey) ->
  F = transit_read_handlers:handler(Tag),
  F(Rep).

decode_hash(Name, AsMapKey) when length(Name) =:= 1 ->
  case Name of
    [{Key, Val}] ->
      case decode(Key, AsMapKey) of
        #tagged_value{tag=Tag} ->
          DecodedVal = decode(Val, AsMapKey),
          decode_tag(Tag, DecodedVal, AsMapKey);
        DecodedKey ->
          Rep = decode(Val, false),
          [{DecodedKey, Rep}]
      end;
    _ ->
      erlang:throw({"unkown hash format: ", Name})
  end;
decode_hash(Name, AsMapKey) ->
  [{decode(Key, AsMapKey), decode(Val, AsMapKey)} || {Key, Val} <- Name].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start_server() ->
  transit_rolling_cache:start_link(),
  ok.

stop_server(ok) ->
  ok = transit_rolling_cache:stop(),
  ok.

unmarshal_test_() ->
  {foreach,
   fun start_server/0,
   fun stop_server/1,
   [fun unmarshal_quoted/1]}.

unmarshal_quoted(ok) ->
  Tests = [{1, <<"[\"~#'\", 1]">>},
           {<<"foo">>, <<"[\"~#'\", \"foo\"]">>},
           {undefined, <<"[\"~#'\", null]">>},
           {true, <<"[\"~#'\", true]">>},
           {false, <<"[\"~#'\", false]">>},
           {transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~m0\"]">>},
           %{transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~t1970-01-01T00:00:01.000Z\"]">>},
           {sets:from_list([<<"foo">>, <<"bar">>, <<"baz">>]), <<"[\"~#set\", [\"foo\",\"bar\",\"baz\"]]">>},
           {[{<<"foo">>, <<"bar">>}], <<"{\"foo\":\"bar\"}">>},
           {[{a, b}, {3, 4}], <<"[\"^ \",\"~:a\",\"~:b\",3,4]">>},
           {[{foo, <<"bar">>}], <<"{\"~:foo\":\"bar\"}">>}
          ],
  [fun() -> Val = decode(jsx:decode(Str), false) end || {Val, Str} <- Tests].

parse_string_test() ->
  ?assertEqual(foo, parse_string(<<"~:foo">>, false)).
-endif.
