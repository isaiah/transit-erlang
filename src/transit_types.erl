-module(transit_types).
-include_lib("transit_types.hrl").
-include_lib("transit_format.hrl").

-export([datetime/1, uri/1, uuid/1, link/1, binary/1, symbol/1, bigint/1]).
-export([list/1, tv/2]).

-type datetime() :: #transit_datetime{}.
-export_type([datetime/0]).

-spec datetime(Timestamp) ->
  DateTime when Timestamp :: erlang:timestamp(), DateTime :: datetime().
datetime({_, _, _} = Timestamp) ->
  #transit_datetime{timestamp=Timestamp}.

uri(URI) ->
  tv(?URI, URI).

link(Link) ->
  tv(?LINK, Link).

uuid(ID) ->
  tv(?UUID, ID).

binary(B) ->
  tv(?BINARY, B).

symbol(S) when is_bitstring(S) ->
  tv(?SYMBOL, S);
symbol(S) when is_list(S) ->
  tv(?SYMBOL, list_to_binary(S));
symbol(S) when is_atom(S) ->
  tv(?SYMBOL, atom_to_binary(S, utf8)).

bigint(I) ->
  tv(?BIGINT, integer_to_binary(I)).

list(A) when is_tuple(A) ->
  tv(?LIST, tuple_to_list(A));
list(A) ->
  tv(?LIST, A).

tv(Tag, Value) ->
  #tagged_value{tag=Tag, rep=Value}.
