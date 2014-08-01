-module(transit_writer).
-behaviour(gen_server).
-include_lib("transit_format.hrl").
-define(SERVER, ?MODULE).

-record(state, {env::transit_marshaler:env(),
                marshaler=transit_json_marshaler :: atom()
               }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/0, stop/0]).
-export([write/1]).

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

init([Format]) ->
  transit_rolling_cache:start_link(),
  ets:new(?TRANSIT_HANDLERS, [set, named_table]),
  Env = transit_marshaler:new_env(),
  Marshaler = case Format of
                json_verbose ->
                  transit_json_verbose_marshaler;
                json ->
                  transit_json_marshaler;
                msgpack ->
                  transit_msgpack_marshaler;
                _ ->
                  erlang:throw("unsupported marshaler: ~s", [Format])
              end,
  {ok, #state{marshaler=Marshaler, env=Env}}.

handle_call({write, Object}, _From, State=#state{marshaler=M, env=Env}) ->
  {Rep, NEnv} = transit_marshaler:marshal_top(M, Object, Env),
  {reply, Rep, State#state{env=NEnv}};

handle_call({register, Record, Handler}, _From, State=#state{}) ->
  ets:insert(?TRANSIT_HANDLERS, {Record, Handler}),
  {reply, ok, State};

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

write(Obj) ->
  gen_server:call(?MODULE, {write, Obj}).
