-module(transit_types).
-include("transit_format.hrl").
-include("transit.hrl").

-export([datetime/1, uri/1, uuid/1, link/1, binary/1, symbol/1, bigint/1]).
-export([list/1, tv/2]).

datetime({_, _, _} = Timestamp) ->
  {timepoint, Timestamp}.

uri(URI) -> {uri, URI}.

link(Link) ->
  tv(?LINK, Link).

uuid(ID) -> {uuid, ID}.

binary(B) -> {binary, B}.

symbol(S) when is_binary(S) -> {sym, S};
symbol(S) when is_list(S) -> {sym, list_to-binary(S)};
symbol(S) when is_atom(S) -> {sym, atom_to_binary(S, utf8)}.

bigint(I) ->
  tv(?BIGINT, integer_to_binary(I)).

list(A) when is_tuple(A) -> {list, tuple_to_list(A)};
list(A) -> {list, A}.

tv(Tag, Value) ->
  #tagged_value{tag=Tag, rep=Value}.
