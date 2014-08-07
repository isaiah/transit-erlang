-module(transit_types).
-export([datetime/1]).

-record(transit_datetime, {timestamp=os:timestamp() :: erlang:timestamp()}).

-type datetime() :: #transit_datetime{}.
-export_type([datetime/0]).

-spec datetime(Timestamp) ->
  DateTime when Timestamp :: erlang:timestamp(), DateTime :: datetime().
datetime(Timestamp) ->
  #transit_datetime{timestamp=Timestamp}.
