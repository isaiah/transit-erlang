-module(transit).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/ 0, write/1, read/1]).

start() ->
  application:start(transit).

stop() ->
  application:stop(transit).

read(Rep) ->
  transit_reader:read(Rep).

write(Rep) ->
  transit_writer:write(Rep).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  transit_sup:start_link().

stop(_State) ->
  ok.
