-module(transit_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, Format} = application:get_env(transit, format),
  {ok, CustomHandler} = application:get_env(transit, write_handler),
  WriterSpec = ?CHILD(transit_writer, worker, [Format, CustomHandler]),
  ReaderSpec = ?CHILD(transit_reader, worker, [Format]),
  {ok, { {one_for_one, 5, 10}, [WriterSpec, ReaderSpec]} }.
