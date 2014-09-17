-module(transit_rolling_cache).
-export([empty/1]).
-export([nocache/0]).

-export([encode/3, decode/3]).
-include("transit_format.hrl").

-compile([{inline, [is_cache_key/1, is_cacheable/2]}]).

-define(CACHE_CODE_DIGITS, 44).
-define(CACHE_SIZE, ?CACHE_CODE_DIGITS * ?CACHE_CODE_DIGITS).
-define(FIRST_ORD, 48).
-define(MIN_SIZE_CACHEABLE, 4).

-type decode_kind() :: key | value.

-record(cache, { kv :: dict:dict(), vk :: dict:dict() }).

-opaque cache() :: #cache{} | nocache.

-export_type([cache/0]).

%% API
%% ---------------------------------------

%% empty/0 returns the new empty cache
-spec empty(Format) -> cache() when Format::atom().
empty(json_verbose) -> nocache;
empty(_Format) -> #cache { kv = dict:new(), vk = dict:new() }.

%% when decoding json verbose, cache is disabled.
nocache() ->
  nocache.

-spec encode(Cache, Name, DecodeKind) -> {Name, Cache}
  when Cache::cache(),
       Name::bitstring(),
       DecodeKind :: decode_kind().
% XXX(isaiah) A very interesting bug of the compiler, this is emitted
% and encode_with_cache doesn't know how to handle "nocache".
%encode(nocache, Name, _AsMapKey) ->
%  {Name, nocahe};
encode(Cache, Name, Kind) ->
  encode_with_cache(Cache, Name, Kind).

decode(#cache { kv = KV } = C, Name, Kind) ->
  case is_cache_key(Name) of
    true ->
      case dict:find(Name, KV) of
        {ok, Val} -> {Val, C};
        error ->
          add_cacheable(C, Name, Kind)
      end;
    false -> add_cacheable(C, Name, Kind)
  end;
%%% no cache
decode(nocache, Name, _Kind) -> {Name, nocache}.

%% INTERNALS
%% ---------------------------------------

add_cacheable(C, Name, Kind) ->
  case is_cacheable(Name, Kind) of
    true -> encache(C, Name);
    false -> {Name, C}
  end.

-spec is_cache_key(binary()) -> boolean().
is_cache_key(<<"^ ", _/binary>>) -> false;
is_cache_key(<<$^, _>>) -> true;
is_cache_key(<<$^, _, _>>) -> true;
is_cache_key(_) -> false.

-spec encode_key(integer()) -> bitstring().
encode_key(Num) ->
  case {Num rem ?CACHE_CODE_DIGITS,
        Num div ?CACHE_CODE_DIGITS} of
    {Lo, 0}  -> <<?SUB/binary, (Lo + ?FIRST_ORD)>>;
    {Lo, Hi} -> <<?SUB/binary, (Hi + ?FIRST_ORD), (Lo + ?FIRST_ORD)>>
  end.

-spec is_cacheable(bitstring(), decode_kind()) -> boolean().
is_cacheable(Str, key)  when byte_size(Str) >= ?MIN_SIZE_CACHEABLE -> true;
is_cacheable(_Str, key) -> false;
is_cacheable(Str, value) when byte_size(Str) >= ?MIN_SIZE_CACHEABLE ->
    case Str of
        <<"~#", _/binary>> -> true;
        <<"~:", _/binary>> -> true;
        <<"~$", _/binary>> -> true;
        _ -> false
    end;
is_cacheable(_Str, value) -> false.

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

encode_with_cache(nocache, Name, _Kind) ->
  {Name, nocache};
encode_with_cache(#cache { kv=KV } = C, Name, Kind) ->
  case dict:find(Name, KV) of
    {ok, Val} -> {Val, C};
    error ->
      case is_cacheable(Name, Kind) of
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
  ?_assert(is_cacheable(<<"~#tag">>, value)),
  ?_assertNot(is_cacheable(<<"~#tag">>, key)),
  ?_assert(is_cacheable(<<"~:foobar">>, key)),
  ?_assert(is_cacheable(<<"foobar">>, key)),
  ?_assertNot(is_cacheable(<<"foobar">>, value)).

encache_test() ->
  {_, C} = encache(empty(json), <<"foobar">>),
  {<<"^0">>, _} = encode_with_cache(C, <<"foobar">>, key).

decode_test_() ->
  Tests = [{<<"~#list">>, value}, {<<"aaaa">>, key}],
  [fun() ->
       Cache = empty(json),
       {_, #cache{} = Cache1} = decode(Cache, Val, AsMapKey),
       {Val, _} = decode(Cache1, <<"^0">>, AsMapKey)
   end || {Val, AsMapKey} <- Tests].
empty_cache_test() ->
  ?_assertEqual(nocache, empty(json_verbose)).
-endif.
