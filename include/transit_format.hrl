%%% ESC = "~"
%%% SUB = "^"
%%% RES = "`"
%%% TAG = "~#"
%%% QUOTE = "'"
%%% MAP_AS_ARR = "^ "
-define(ESC, "~").
-define(SUB, "^").
-define(RES, "`").
-define(TAG, "~#").
-define(QUOTE, "'").
-define(MAP_AS_ARR, "^ ").

-define(Null, "_").
-define(Int, "i").
-define(BigInt, "n").
-define(Float, "f").
-define(String, "s").
-define(Boolean, "?").
-define(Array, "array").
-define(Map, "map").

-define(TRANSIT_HANDLERS, transit_handlers).

-record(write_handler, {tag :: fun((any()) -> string()),
                        rep :: fun((any()) -> string()),
                        string_rep :: fun((any()) -> string())}).
