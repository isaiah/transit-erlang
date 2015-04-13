-module(eqc_lib).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

sign() -> elements([1, -1]).

pow(0, 0) -> 0;
pow(_Base, 0) -> 1;
pow(Base, N) -> Base * pow(Base, N-1).
  
interesting_int() ->
    ?LET({Sign, Exponent, Perturb}, {sign(), choose(0, 128), choose(-3, 3)},
        Sign * pow(2, Exponent) + Perturb).

hex_char() ->
    elements([$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $a, $b, $c, $d, $e, $f]).

hex_string(N) ->
    vector(N, hex_char()).

uuid() ->
    ?LET({S1, S2, S3, S4, S5}, {hex_string(8), hex_string(4), hex_string(4), hex_string(4), hex_string(12)},
         iolist_to_binary([S1, $-, S2, $-, S3, $-, S4, $-, S5])).

non_character_codepoint(CP) when CP >= 16#FDD0, CP =< 16#FDEF -> true;
non_character_codepoint(CP) ->
    case 16#FFFF band CP of
        16#FFFE -> true;
        16#FFFF -> true;
        _ -> false
    end.
    
surrogate_codepoint(CP) when CP >= 16#D800, CP =< 16#DFFF -> true;
surrogate_codepoint(_CP) -> false.

valid_codepoint(CP) ->
    not (non_character_codepoint(CP) orelse surrogate_codepoint(CP)).

%% code_point/0 generates a valid utf-8 code point.
%% There is a plane which is not allowed, so kill it
code_point_bmp() ->
    ?SUCHTHAT(CP, choose(0, 1000*1000),
        valid_codepoint(CP)).

code_point_supplementary() ->
    ?SUCHTHAT(CP, choose(16#010000, 16#10FFFF),
        valid_codepoint(CP)).
    
 code_point() ->
     frequency([
       {100, code_point_bmp()},
       {1, code_point_supplementary()} ]).

utf8_string_bmp() ->
    ?LET(CodePoints, list(code_point()),
        unicode:characters_to_binary(CodePoints)).

utf8_string() ->
  utf8_string_bmp().
  
all_utf8(<<>>) -> true;
all_utf8(<<_/utf8, N/binary>>) -> all_utf8(N);
all_utf8(_) -> false.

prop_utf8_string_correct() ->
    ?FORALL(S, utf8_string_bmp(),
      all_utf8(S)).

uri() ->
    oneof([<<"http://foo/bar">>,
      <<"https://quux.zot/blemphf?query=albo#frags">>
    ]).
