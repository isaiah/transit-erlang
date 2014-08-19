-module(transit_rolling_cache).
-export([empty/0]).

-export([encode/3, decode/3]).
-include_lib("transit_format.hrl").

-define(CACHE_CODE_DIGITS, 44).
-define(CACHE_SIZE, ?CACHE_CODE_DIGITS * ?CACHE_CODE_DIGITS).
-define(FIRST_ORD, 48).
-define(MIN_SIZE_CACHEABLE, 4).

-record(cache, { kv :: dict:dict(), vk :: dict:dict() }).

-opaque cache() :: #cache{}.

-export_type([cache/0]).

%% API
%% ---------------------------------------

%% empty/0 returns the new empty cache
-spec empty() -> cache().
empty() -> #cache { kv = dict:new(), vk = dict:new() }.

-spec encode(Cache, Name, boolean()) -> Name
  when Cache::cache(), Name::bitstring().
encode(Cache, Name, AsMapKey) ->
  encode_with_cache(Cache, Name, AsMapKey).

decode(#cache { kv = KV } = C, Name, AsMapKey) ->
  case is_cache_key(Name) of
    true ->
      case dict:find(Name, KV) of
        {ok, Val} -> {Val, C};
        error ->
          add_cacheable(C, Name, AsMapKey)
      end;
    false -> add_cacheable(C, Name, AsMapKey)
  end.

%% INTERNALS
%% ---------------------------------------

add_cacheable(C, Name, AsMapKey) ->
  case is_cacheable(Name, AsMapKey) of
    true -> encache(C, Name);
    false -> {Name, C}
  end.

-spec is_cache_key(binary()) -> boolean().
is_cache_key(<<"^ ", _/binary>>) -> false;
is_cache_key(<<"^", _/binary>> = Name) when byte_size(Name) >= 2 -> true;
is_cache_key(_) -> false.

-spec encode_key(integer()) -> bitstring().
encode_key(Num) ->
  case {Num rem ?CACHE_CODE_DIGITS,
        Num div ?CACHE_CODE_DIGITS} of
    {Lo, 0}  -> <<?SUB/binary, (Lo + ?FIRST_ORD)>>;
    {Lo, Hi} -> <<?SUB/binary, (Hi + ?FIRST_ORD), (Lo + ?FIRST_ORD)>>
  end.

-spec is_cacheable(bitstring(), boolean()) -> boolean().
is_cacheable(Str, true)  when byte_size(Str) >= ?MIN_SIZE_CACHEABLE -> true;
is_cacheable(_Str, true) -> false;
is_cacheable(Str, false) when byte_size(Str) >= ?MIN_SIZE_CACHEABLE ->
    case binary:match(Str, [<<"~#">>, <<"~:">>, <<"~$">>]) of
      nomatch -> false;
      {_, _} -> true
    end;
is_cacheable(_Str, false) -> false.

encache(#cache { kv=KV } = C, N) ->
  case dict:size(KV) > ?CACHE_SIZE of
    true -> encache_(C#cache { vk = dict:new() }, N);
    false -> encache_(C, N)
  end.
  
encache_(#cache { kv=KV, vk=VK } = C, N) ->
  case dict:find(N, VK) of
    {ok, Val} -> {Val, C};
    error ->
      Key = encode_key(dict:size(KV)),
      {N, C#cache { kv = dict:store(Key, N, KV),
                    vk = dict:store(N, Key, VK) }}
  end.

encode_with_cache(#cache { kv=KV } = C, Name, AsMapKey) ->
  case dict:find(Name, KV) of
    {ok, Val} -> {Val, C};
    error ->
      case is_cacheable(Name, AsMapKey) of
        true ->
          encache(C, Name);
        false ->
          {Name, C}
      end
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_cache_key_test_() ->
  ?_assert(is_cache_key(<<"^0">>)),
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
  {_, C} = encache(empty(), <<"foobar">>),
  {<<"^0">>, _} = encode_with_cache(C, <<"foobar">>, true).

decode_test() ->
  Val = <<"~#list">>,
  Cache = empty(),
  {_, #cache{} = Cache1} = decode(Cache, Val, false),
  {Val, _} = decode(Cache1, <<"^0">>, false).

-endif.
