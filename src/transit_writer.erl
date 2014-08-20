-module(transit_writer).
-export([write/2]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

write(Obj, [{format, Format}, {handler, CustomHandler}|_Config]) ->
  Marshaler = case Format of
                json_verbose ->
                  transit_json_verbose_marshaler;
                json ->
                  transit_json_marshaler;
                msgpack ->
                  transit_msgpack_marshaler;
                _ ->
                  exit(unsupported_handler)
              end,
  transit_marshaler:marshal_top(Marshaler, Obj, {Format, CustomHandler});
write(Obj, [{format, Format}|Config]) ->
  write(Obj, [{format, Format}, {handler, ?MODULE}|Config]).
