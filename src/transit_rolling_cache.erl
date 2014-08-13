-module(transit_rolling_cache).
-export([encode/3, decode/3]).
-include_lib("transit_format.hrl").

-define(CACHE_CODE_DIGITS, 44).
-define(FIRST_ORD, 48).
-define(MIN_SIZE_CACHEABLE, 4 * 8).

-spec encode(Cache, Name, boolean()) -> Name when Cache::pid(), Name::bitstring().
encode(Cache, Name, AsMapKey) ->
  encode_with_cache(Name, AsMapKey, Cache).

decode(C={Kv, _}, Name, AsMapKey) ->
  case is_cache_key(Name) of
    true ->
      case dict:find(Name, Kv) of
        {ok, Val} ->
          {Val, C};
        _ ->
          case is_cacheable(Name, AsMapKey) of
            true ->
              {Val, C1} = encache(Name, C),
              {Val, C1};
            false ->
              {Name, C}
          end
      end;
    false ->
      case is_cacheable(Name, AsMapKey) of
        true ->
          {Val, C2} = encache(Name, C),
          {Val, C2};
        false ->
          {Name, C}
      end
  end.

-spec is_cache_key(bitstring()) -> boolean().
is_cache_key(Name) when bit_size(Name) > 16 ->
  <<Sub, Ext, _Tail/binary>> = Name,
  case Sub of
    $~ ->
      case Ext of
        32 -> false; %%% space
        _ -> true
      end;
    _ -> false
  end;
is_cache_key(_) ->
  false.

-spec encode_key(integer()) -> bitstring().
encode_key(Num) ->
  Lo = Num rem ?CACHE_CODE_DIGITS,
  Hi = Num div ?CACHE_CODE_DIGITS,
  if Hi =:= 0 ->
       Lbit = Lo + ?FIRST_ORD,
       <<?SUB/bitstring, Lbit>>;
     true ->
       Lbit = Lo + ?FIRST_ORD,
       Hbit = Hi + ?FIRST_ORD,
       <<?SUB/bitstring, Hbit, Lbit>>
  end.

%-spec decode_key(string()) -> integer().
%decode_key(Str) ->
%  case string:len(Str) of
%    2 ->
%      ord(lists:nth(2, Str)) - ?FIRST_ORD;
%    _ ->
%      (ord(lists:nth(3, Str)) - ?FIRST_ORD) + ?CACHE_CODE_DIGITS * (ord(lists:nth(2, Str)) - ?FIRST_ORD)
%  end.

-spec is_cacheable(bitstring(), boolean()) -> boolean().
is_cacheable(Str, true) ->
  if bit_size(Str) >= ?MIN_SIZE_CACHEABLE ->
       true;
     true ->
       false
  end;
is_cacheable(Str, false) ->
  if bit_size(Str) >= ?MIN_SIZE_CACHEABLE ->
       case binary:match(Str, [<<"~#">>, <<"~:">>, <<"~$">>]) of
         nomatch -> false;
         _ -> true
       end;
     true ->
       false
  end.

%-spec ord(char()) -> integer().
%ord(Char) ->
%  [Int] = io_lib_format:fwrite("~w", Char),
%  Int.

encache(Name, C={Kv,Vk}) ->
  case dict:find(Name, Vk) of
    {ok, Val} ->
      {Val, C};
    error ->
      Key = encode_key(dict:size(Kv)),
      {Name, {dict:store(Key, Name, Kv), dict:store(Name, Key, Vk)}}
  end.

encode_with_cache(Name, AsMapKey, C={Kv, _}) ->
  case dict:find(Name, Kv) of
    {ok, Val} ->
      {Val, C};
    error ->
      case is_cacheable(Name, AsMapKey) of
        true ->
          encache(Name, C);
        false ->
          {Name, C}
      end
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_cache_key_test_() ->
  ?_assert(is_cache_key(<<"^123">>)),
  ?_assertNot(is_cache_key(<<"^ 123">>)),
  ?_assertNot(is_cache_key(<<"123">>)),
  ?_assertNot(is_cache_key(<<"">>)).

is_cacheable_test_() ->
  ?_assert(is_cacheable(<<"~#tag">>, false)),
  ?_assertNot(is_cacheable(<<"~#tag">>, true)),
  ?_assert(is_cacheable(<<"~:foobar">>, true)),
  ?_assert(is_cacheable(<<"foobar">>, true)),
  ?_assertNot(is_cacheable(<<"foobar">>, false)).

encache_test() ->
  {_, C} = encache(<<"foobar">>, {dict:new(), dict:new()}),
  {<<"^0">>, _} = encode_with_cache(<<"foobar">>, true, C).
-endif.
