-module(transit).
-export([read/1, read/2, write/1, write/2]).

read(Rep) ->
	read(Rep, #{ format => json }).

read(Rep, Opts) ->
  transit_reader:read(Rep, Opts).

write(Rep) ->
	write(Rep, #{ format => json }).
	
write(Rep, Opts) ->
  transit_writer:write(Rep, Opts).
