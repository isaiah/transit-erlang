-module(transit).
-export([start/0, stop/ 0, write/1, read/1]).

start() ->
  application:start(transit).

stop() ->
  application:stop(transit).

read(Rep) ->
  transit_reader:read(Rep).

write(Rep) ->
  transit_writer:write(Rep).
