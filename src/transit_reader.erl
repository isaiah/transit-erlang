-module(transit_reader).
-behaviour(gen_server).
-include_lib("transit_format.hrl").

-define(SERVER, ?MODULE).

-record(state, {env::transit_marshaler:env(),
                unmarshaler=transit_json_unmarshaler}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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
  transit_rolling_cache:start_link(),
  State = #state{},
  {ok, State}.

handle_call({read, Name}, _From, State) ->
  {Val, S1} = decode_string(Name, State),
  {reply, Val, S1};
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

decode_string(String, State=#state{env=Env}) ->
  parse_string(transit_rolling_cache:decode(String, transit_marshaler:as_map_key(Env)), State).

parse_string(String, State=#state{}) ->
  Val = case String of
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
        end,
  {Val, State}.

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
   []}.

unmarshal_string(ok) ->
  Env = transit_marshaler:new_env(),
  Tests = [{1, "[\"~#'\", 1]"}],
  [fun() -> {Val, _} = decode_string(Str, #state{env=Env}) end || {Val, Str} <- Tests].
-endif.
