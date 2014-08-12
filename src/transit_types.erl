-module(transit_types).
-include_lib("transit_types.hrl").
-include_lib("transit_format.hrl").

-export([datetime/1, uri/1, uuid/1, link/1]).

-type datetime() :: #transit_datetime{}.
-export_type([datetime/0]).

-spec datetime(Timestamp) ->
  DateTime when Timestamp :: erlang:timestamp(), DateTime :: datetime().
datetime({_, _, _} = Timestamp) ->
  #transit_datetime{timestamp=Timestamp}.

uri(URI) ->
  tv(?URI, URI).

link(Link) ->
  tv(?Link, Link).

uuid(ID) ->
  tv(?UUID, ID).

tv(Tag, Value) ->
  #tagged_value{tag=Tag, rep=Value}.
