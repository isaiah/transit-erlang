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
  {ok, Cache} = transit_rolling_cache:start(),
  {ok, Cache}.

handle_call({read, Name}, _From, Cache) ->
  Val = decode(Cache, Name, false),
  {reply, Val, Cache};
handle_call(_Request, _From, Cache) ->
  {reply, ok, Cache}.

handle_cast(_Msg, Cache) ->
  {noreply, Cache}.

handle_info(_Info, Cache) ->
  {noreply, Cache}.

terminate(_Reason, _Cache) ->
  ok.

code_change(_OldVsn, Cache, _Extra) ->
  {ok, Cache}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
decode(Cache, Name, AsMapKey) when is_bitstring(Name) ->
  decode_string(Cache, Name, AsMapKey);
decode(Cache, Name, AsMapKey) when is_list(Name) ->
  case Name of
    [?MAP_AS_ARR|Tail] ->
      decode_array_hash(Cache, Tail, AsMapKey);
    [{_, _}|_] ->
      decode_hash(Cache, Name, AsMapKey);
    [EscapedTag, Rep] ->
      <<"~", "#", Tag/binary>> = EscapedTag,
      decode_tag(Tag, decode(Cache, Rep, AsMapKey), AsMapKey);
    _ ->
      decode_array(Cache, Name, AsMapKey)
  end;
decode(_Cache, Name, _AsMapKey) ->
  Name.

decode_string(Cache, String, AsMapKey) ->
  parse_string(transit_rolling_cache:decode(Cache, String, AsMapKey), AsMapKey).

parse_string(String, _Cache) ->
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

decode_array_hash(Cache, [Key,Val|Name], AsMapKey) ->
  [{decode(Cache, Key, true), decode(Cache, Val, AsMapKey)}|decode_array_hash(Cache, Name, AsMapKey)];
decode_array_hash(_Cache, [], _AsMapKey) ->
  [].

decode_array(Cache, Name, AsMapKey) ->
  [decode(Cache, El, AsMapKey) || El <- Name].

decode_tag(Tag, Rep, _Cache) ->
  F = transit_read_handlers:handler(Tag),
  F(Rep).

decode_hash(Cache, Name, AsMapKey) when length(Name) =:= 1 ->
  case Name of
    [{Key, Val}] ->
      case decode(Cache, Key, AsMapKey) of
        #tagged_value{tag=Tag} ->
          DecodedVal = decode(Cache, Val, AsMapKey),
          decode_tag(Tag, DecodedVal, AsMapKey);
        DecodedKey ->
          Rep = decode(Cache, Val, false),
          [{DecodedKey, Rep}]
      end;
    _ ->
      erlang:throw({"unkown hash format: ", Name})
  end;
decode_hash(Cache, Name, AsMapKey) ->
  [{decode(Cache, Key, AsMapKey), decode(Cache, Val, AsMapKey)} || {Key, Val} <- Name].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
start_server() ->
  {ok, C} = transit_rolling_cache:start(),
  C.

stop_server(C) ->
  ok = transit_rolling_cache:stop(C),
  ok.

unmarshal_test_() ->
  {foreach,
   fun start_server/0,
   fun stop_server/1,
   [fun unmarshal_quoted/1]}.

unmarshal_quoted(C) ->
  Tests = [{1, <<"[\"~#'\", 1]">>},
           {<<"foo">>, <<"[\"~#'\", \"foo\"]">>},
           {undefined, <<"[\"~#'\", null]">>},
           {true, <<"[\"~#'\", true]">>},
           {false, <<"[\"~#'\", false]">>},
           {transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~m0\"]">>},
           %{transit_types:datetime({0,0,0}), <<"[\"~#'\",\"~t1970-01-01T00:00:01.000Z\"]">>},
           {sets:from_list([<<"foo">>, <<"bar">>, <<"baz">>]), <<"[\"~#set\", [\"foo\",\"bar\",\"baz\"]]">>},
           {[{<<"foo">>, <<"bar">>}], <<"{\"foo\":\"bar\"}">>},
           {[{<<"a">>, <<"b">>}, {3, 4}], <<"[\"^ \",\"a\",\"b\",3,4]">>},
           {[{a, b}, {3, 4}], <<"[\"^ \",\"~:a\",\"~:b\",3,4]">>},
           {[{foo, <<"bar">>}], <<"{\"~:foo\":\"bar\"}">>}
          ],
  [fun() -> Val = decode(C, jsx:decode(Str), false) end || {Val, Str} <- Tests].

parse_string_test() ->
  ?assertEqual(foo, parse_string(<<"~:foo">>, false)).
-endif.
