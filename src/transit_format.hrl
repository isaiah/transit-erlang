-define(ESC, <<"~">>).
-define(SUB, <<"^">>).
-define(RES, <<"`">>).
-define(TAG, <<"~#">>).
-define(QUOTE, <<"'">>).
-define(MAP_AS_ARR, <<"^ ">>).

-define(ARRAY, <<"array">>).
-define(BIGINT, <<"n">>).
-define(BINARY, <<"b">>).
-define(BOOLEAN, <<"?">>).
-define(CMAP, <<"cmap">>).
-define(DATE, <<"m">>).
-define(FLOAT, <<"d">>).
-define(INT, <<"i">>).
-define(KEYWORD, <<":">>).
-define(LINK, <<"link">>).
-define(LIST, <<"list">>).
-define(MAP, <<"map">>).
-define(NULL, <<"_">>).
-define(SET, <<"set">>).
-define(SPECIAL_NUMBER, <<"z">>).
-define(STRING, <<"s">>).
-define(SYMBOL, <<"$">>).
-define(URI, <<"r">>).
-define(UUID, <<"u">>).
-define(VERBOSEDATE, <<"t">>).

-define(TRANSIT_HANDLERS, transit_handlers).

-record(env, {as_map_key=false :: boolean(),
              cache :: transit_rolling_cache:cache(),
              custom_handler :: module()
             }).
