-module(transit_write_handler).

-include_lib("transit.hrl").
-include("transit_format.hrl").

-callback handler(Rep) -> Handler when Rep::term(), Handler::#write_handler{}.
