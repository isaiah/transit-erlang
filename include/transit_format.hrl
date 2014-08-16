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
-define(Float, <<"d">>).
-define(String, <<"s">>).
-define(Boolean, <<"?">>).
-define(Keyword, <<":">>).
-define(Date, <<"m">>).
-define(VerboseDate, <<"t">>).
-define(UUID, <<"u">>).
-define(URI, <<"r">>).
-define(Binary, <<"b">>).
-define(Symbol, <<"$">>).
-define(Array, <<"array">>).
-define(Link, <<"link">>).
-define(List, <<"list">>).
-define(Map, <<"map">>).
-define(Set, <<"set">>).

-define(TRANSIT_HANDLERS, transit_handlers).

-record(write_handler, {tag :: fun((any()) -> string()),
                        rep :: fun((any()) -> string()),
                        string_rep :: fun((any()) -> bitstring())}).

-record(tagged_value, {tag :: bitstring(),
                       rep :: any()}).

-type tagged_value() :: #tagged_value{}.

-record(env, {started = queue:new() :: queue:queue(boolean()),
              is_key = queue:new() :: queue:queue(boolean()),
              as_map_key=false :: boolean(),
              cache :: pid(),
              custom_handler :: module()
             }).

-type env()::#env{}.
-export_type([env/0]).


