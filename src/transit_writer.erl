-module(transit_writer).
-export([write/2]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

write(Obj, [{format, Format}, {handler, CustomHandler}|_Config]) ->
  Rep = transit_marshaler:marshal_top(marshaler(Format), Obj, {Format, CustomHandler}),
  pack(Format, Rep);
write(Obj, [{format, Format}|Config]) ->
  write(Obj, [{format, Format}, {handler, ?MODULE}|Config]).

marshaler(json_verbose) -> transit_json_verbose_marshaler;
marshaler(json) -> transit_json_marshaler;
marshaler(msgpack) -> transit_json_marshaler.

pack(msgpack, Rep) -> msgpack:pack(Rep, [{format, jsx}]);
pack(json, Rep) -> jsx:encode(Rep);
pack(json_verbose, Rep) -> jsx:encode(Rep).

