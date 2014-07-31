-module(transit_verbose_json_marshaler).
-behaviour(transit_marshaler).

emit_map(M, S) ->
  {MapStart, S1} = transit_marshaler:emit_map_start(S),
  {Body, S2} = maps:fold(fun (K, V, {In, NS1}) ->
                        {MK, NS2} = transit_marshaler:marshal(?MODULE, K, NS1),
                        {MV, NS3} = transit_marshaler:marshal(?MODULE, V, NS2),
                        {<<In/bitstring, MK/bitstring, MV/bitstring>>, NS3}
                    end,
                    {<<>>, S1}, M),
  {MapEnd, S3} = transit_marshaler:emit_map_end(S2),
  {<<MapStart/bitstring, Body/bitstring, MapEnd/bitstring>>, S3}.
