-module(transit_rolling_cache).
-compile(export_all).
-include_lib("transit_format.hrl").

-define(CACHE_CODE_DIGITS, 44).
-define(FIRST_ORD, 48).
-define(MIN_SIZE_CACHEABLE, 4).

-spec is_cache_key(string()) -> boolean().
is_cache_key(Name) when length(Name) > 0 ->
  case lists:prefix(?SUB, Name) of
    true ->
      case lists:prefix(?MAP_AS_ARR, Name) of
        true -> false;
        false -> true
      end;
    false -> false
  end;
is_cache_key(_) -> false.

-spec encode_key(integer()) -> string().
encode_key(Num) ->
  Lo = Num rem ?CACHE_CODE_DIGITS,
  Hi = Num div ?CACHE_CODE_DIGITS,
  if Hi =:= 0 ->
       ?SUB ++ integer_to_list(Lo + ?FIRST_ORD);
     true ->
       ?SUB ++ integer_to_list(Hi + ?FIRST_ORD) + integer_to_list(Lo + ?FIRST_ORD)
  end.

-spec decode_key(string()) -> integer().
decode_key(Str) ->
  case string:len(Str) of
    2 ->
      ord(lists:nth(2, Str)) - ?FIRST_ORD;
    _ ->
      (ord(lists:nth(3, Str)) - ?FIRST_ORD) + ?CACHE_CODE_DIGITS * (ord(lists:nth(2, Str)) - ?FIRST_ORD)
  end.

-spec is_cacheable(string(), boolean()) -> boolean().
is_cacheable(Str, true) when length(Str) >= ?MIN_SIZE_CACHEABLE ->
  case string:substr(Str, 1, 2) of
    "~#" -> true;
    "~:" -> true;
    "~$" -> true;
    _ -> false
  end;
is_cacheable(_, _) -> false.

-spec ord(char()) -> integer().
ord(Char) ->
  [Int] = io_lib_format:fwrite("~w", Char),
  Int.
