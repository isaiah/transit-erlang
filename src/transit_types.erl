-module(transit_types).
-include_lib("transit_types.hrl").

-export([datetime/1]).

-type datetime() :: #transit_datetime{}.
-export_type([datetime/0]).

-spec datetime(Timestamp) ->
  DateTime when Timestamp :: erlang:timestamp(), DateTime :: datetime().
datetime({_, _, _} = Timestamp) ->
  #transit_datetime{timestamp=Timestamp}.

