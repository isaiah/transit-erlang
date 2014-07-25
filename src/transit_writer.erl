-module(transit_writer).
-compile(export_all).

new(io, protocol, opts) ->
  {?MODULE, [io, protocol, opts]}.
