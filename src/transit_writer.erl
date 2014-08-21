-module(transit_writer).
-export([write/2]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

write(Obj, [{format, Format}, {handler, CustomHandler}|_Config]) ->
  Marshaler = case Format of
                json_verbose ->
                  transit_json_verbose_marshaler;
                _ ->
                  transit_json_marshaler
              end,
  Rep = transit_marshaler:marshal_top(Marshaler, Obj, {Format, CustomHandler}),
  case Format of
    msgpack ->
      msgpack:pack(Rep, [{format, jsx}]);
    _ ->
      jsx:encode(Rep)
  end;
write(Obj, [{format, Format}|Config]) ->
  write(Obj, [{format, Format}, {handler, ?MODULE}|Config]).
