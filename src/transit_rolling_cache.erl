-module(transit_rolling_cache).
-behavior(gen_server).
-export([encode/1, decode/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-include_lib("transit_format.hrl").

-define(CACHE_CODE_DIGITS, 44).
-define(FIRST_ORD, 48).
-define(MIN_SIZE_CACHEABLE, 4).

-record(cache, {kv=dict:new() :: dict:dict(),
                vk=dict:new() :: dict:dict()}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #cache{}}.

encode(Name) ->
  gen_server:call(?MODULE, {encode, Name}).

decode(Name) ->
  gen_server:call(?MODULE, {decode, Name}).

handle_call({encode, Name}, _From, C=#cache{kv=Kv}) ->
  case dict:find(Name, Kv) of
    {ok, Val} ->
      {reply, Val, C};
    error ->
      case is_cacheable(Name) of
        true ->
          {Val, C1} = encache(Name, C),
          {reply, Val, C1};
        false ->
          {reply, Name, C}
      end
  end;

handle_call({decode, Name}, _From, C=#cache{kv=Kv}) ->
  case is_cache_key(Name) of
    true ->
      case dict:find(Name, Kv) of
        {ok, Val} ->
          {reply, Val, C};
        _ ->
          case is_cacheable(Name) of
            true ->
              {Val, C1} = encache(Name, C),
              {reply, Val, C1};
            false ->
              {reply, Name, C}
          end
      end;
    false ->
      case is_cacheable(Name) of
        true ->
          {Val, C2} = encache(Name, C),
          {reply, Val, C2};
        false ->
          {reply, Name, C}
      end
  end;
handle_call(_Msg, _From, C) ->
  {reply, ok, C}.

handle_cast(_Msg, C) ->
  {noreply, C}.

handle_info(_Info, C) ->
  {noreply, C}.

code_change(_OldVsn, C, _Extra) ->
  {ok, C}.

terminate(_Rsn, _C) ->
  ok.


-spec is_cache_key(bitstring()) -> boolean().
is_cache_key(Name) when bit_size(Name) > 0 ->
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

-spec encode_key(integer()) -> string().
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

-spec decode_key(string()) -> integer().
decode_key(Str) ->
  case string:len(Str) of
    2 ->
      ord(lists:nth(2, Str)) - ?FIRST_ORD;
    _ ->
      (ord(lists:nth(3, Str)) - ?FIRST_ORD) + ?CACHE_CODE_DIGITS * (ord(lists:nth(2, Str)) - ?FIRST_ORD)
  end.

-spec is_cacheable(as_map_key, bitstring()) -> true.
is_cacheable(as_map_key, Str) when bit_size(Str) >= ?MIN_SIZE_CACHEABLE ->
  true.

-spec is_cacheable(bitstring()) -> boolean().
is_cacheable(Str) when bit_size(Str) >= ?MIN_SIZE_CACHEABLE ->
  case binary:match(Str, [<<"~#">>, <<"~:">>, <<"~$">>]) of
    nomatch -> false;
    _ -> true
  end;
is_cacheable(_) -> false.

-spec ord(char()) -> integer().
ord(Char) ->
  [Int] = io_lib_format:fwrite("~w", Char),
  Int.

encache(Name, C=#cache{kv=Kv, vk=Vk}) ->
  case dict:find(Name, Vk) of
    {ok, Val} ->
      {Val, C};
    error ->
      Key = encode_key(dict:size(Kv)),
      {Name, C#cache{kv=dict:store(Key, Name, Kv), vk=dict:store(Name, Key, Vk)}}
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_cache_key_test_() ->
  ?_assert(is_cache_key(<<"^123">>)),
  ?_assertNot(is_cache_key(<<"^ 123">>)),
  ?_assertNot(is_cache_key(<<"123">>)),
  ?_assertNot(is_cache_key(<<"">>)).

is_cacheable_test() ->
  ?assert(is_cacheable(<<"~#tag">>)).
-endif.
