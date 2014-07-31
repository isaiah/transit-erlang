%%% ESC = "~"
%%% SUB = "^"
%%% RES = "`"
%%% TAG = "~#"
%%% QUOTE = "'"
%%% MAP_AS_ARR = "^ "
-define(ESC, <<"~">>).
-define(SUB, <<"^">>).
-define(RES, <<"`">>).
-define(TAG, <<"~#">>).
-define(QUOTE, <<"'">>).
-define(MAP_AS_ARR, <<"^ ">>).

-define(Null, <<"_">>).
-define(Int, <<"i">>).
-define(BigInt, <<"n">>).
-define(Float, <<"f">>).
-define(String, <<"s">>).
-define(Boolean, <<"?">>).
-define(Keyword, <<":">>).
-define(UUID, <<"u">>).
-define(URI, <<"r">>).
-define(Array, <<"array">>).
-define(Map, <<"map">>).

-define(TRANSIT_HANDLERS, transit_handlers).

-record(write_handler, {tag :: fun((any()) -> string()),
                        rep :: fun((any()) -> string()),
                        string_rep :: fun((any()) -> string())}).

-record(tagged_value, {tag :: bitstring(),
                       rep :: bitstring(),
                       string_rep = undefined :: bitstring() | atom()}).

-type tagged_value() :: #tagged_value{}.
