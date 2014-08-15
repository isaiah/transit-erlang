-module(transit).
-export([write/2, read/2]).

read(Rep, Opts) ->
  transit_reader:read(Rep, Opts).

write(Rep, Opts) ->
  transit_writer:write(Rep, Opts).
