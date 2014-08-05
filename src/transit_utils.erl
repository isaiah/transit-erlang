-module(transit_utils).
-export([is_set/1]).

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
