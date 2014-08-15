-module(transit_utils).
-export([is_set/1]).
-export([iso_8601_fmt/1]).
-export([ms_to_date/1]).
-export([ms_to_timestamp/1]).

is_set(Data) ->
  case ordsets:is_set(Data) of
    true ->
      ordset;
    false ->
      case sets:is_set(Data) of
        true ->
          sets;
        false ->
          case gb_sets:is_set(Data) of
            true ->
              gb_sets;
            false ->
              undefined
          end
      end
  end.

iso_8601_fmt(Timestamp) ->
  {_, _, Low} = Timestamp,
  Millis = Low rem 1000,
  DateTime = calendar:now_to_datetime(Timestamp),
  {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
      [Year, Month, Day, Hour, Min, Sec, Millis]).

ms_to_date(Milliseconds) ->
   BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   Seconds       = BaseDate + (Milliseconds div 1000),
   calendar:gregorian_seconds_to_datetime(Seconds).

ms_to_timestamp(Milliseconds) ->
  {Milliseconds div 1000000000,
   Milliseconds div 10000 rem 100000,
   Milliseconds rem 10000}.
