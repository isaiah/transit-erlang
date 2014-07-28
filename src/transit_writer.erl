-module(transit_writer).
-behaviour(gen_server).
-include_lib("transit_format.hrl").
-define(SERVER, ?MODULE).

-record(state, {marshaler=json}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([write/1, register/2]).

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

init([json]) ->
  transit_json_marshaler:start_link(),
  ets:new(?TRANSIT_HANDLERS, [set, named_table]),
  {ok, #state{marshaler=transit_json_marshaler}}.

handle_call({write, Obj}, _From, State=#state{marshaler=M}) ->
  M:marshal_top(Obj),
  {reply, ok, State};
handle_call({register, Record, Handler}, _From, State=#state{}) ->
  ets:insert(?TRANSIT_HANDLERS, {Record, Handler}),
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

write(Obj) ->
  gen_server:call(?MODULE, {write, Obj}).

register(Record, Handler) ->
  gen_server:call(?MODULE, {register, Record, Handler}).
