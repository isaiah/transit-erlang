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

-define(Array, <<"array">>).
-define(BigInt, <<"n">>).
-define(Binary, <<"b">>).
-define(Boolean, <<"?">>).
-define(CMap, <<"cmap">>).
-define(Date, <<"m">>).
-define(Float, <<"d">>).
-define(Int, <<"i">>).
-define(Keyword, <<":">>).
-define(Link, <<"link">>).
-define(List, <<"list">>).
-define(Map, <<"map">>).
-define(Null, <<"_">>).
-define(Set, <<"set">>).
-define(SPECIAL_NUMBER, <<"z">>).
-define(String, <<"s">>).
-define(Symbol, <<"$">>).
-define(URI, <<"r">>).
-define(UUID, <<"u">>).
-define(VerboseDate, <<"t">>).


-define(TRANSIT_HANDLERS, transit_handlers).

-record(write_handler, {tag :: fun((any()) -> string()),
                        rep :: fun((any()) -> string()),
                        string_rep :: fun((any()) -> bitstring())}).

-record(tagged_value, {tag :: bitstring(),
                       rep :: any()}).

-type tagged_value() :: #tagged_value{}.

-record(env, {as_map_key=false :: boolean(),
              cache :: transit_rolling_cache:cache(),
              custom_handler :: module()
             }).

-type env()::#env{}.
-export_type([env/0]).


