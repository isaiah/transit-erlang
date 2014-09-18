[![Build
Status](https://travis-ci.org/isaiah/transit-erlang.svg)](https://travis-ci.org/isaiah/transit-erlang)

transit-erlang
==============
[transit-format](https://github.com/cognitect/transit-format) implementation in Erlang.

Test and developed on Erlang/OTP R17.

*NOTE:* Things are still experimental and subject to change.

Usage
-----

```shell
rebar get-deps compile
erl -pa ebin deps/*/ebin
```

```erlang
A = transit:write(#{<<"a">> => <<"b">>, 3 => 4}, #{ format => json }).
%% => <<"[\"^ \",\"a\",\"b\",3,4]">>
transit:read(A, [{format, json}]).
%% => #{<<"a">> => <<"b">>, 3 => 4}

%%% JSON Verbose mode
transit:write(#{<<"a">> => <<"b">>, 3 => 4}, #{ format => json_verbose }).
%% => <<"{\"~i3\":4,\"a\":\"b\"}">>

%%% msgpack
transit:write(#{<<"a">> => <<"b">>, 3 => 4}, #{ format => msgpack }).
%% => <<149,162,94,32,161,97,161,98,163,126,105,51,4>>
```

Benchmarks
--------------------

These benchmarks are run on a Lenovo Thinkpad W540 with a 16 Gigabyte RAM configuration and the following CPU core:

	Intel(R) Core(TM) i7-4900MQ CPU @ 2.80GHz

Timings run 300 rounds of encoding of the file `transit-format/examples/0.8/example.json` and then we divide down to get the
encoder time for each round. This then forms the base benchmark.

| Commit | Test |  Timing ms |
| ------ | ---- | ------ |
| 3d3b04e | JSON | #{iso => 31.987, read => 9.976, write => 20.810} |
| c976ce6 | JSON | #{iso => 29.248, read => 8.883, write => 18.700} |
| 9d678c8 | JSON | #{iso => 26.6893,read => 7.454,write => 18.178} |
| 3d3b04e | MsgPack | #{iso => 15.911, read => 4.901, write => 12.072} |
| c976ce6 | MsgPack | #{iso => 11.713, read => 3.258, write => 9.051} |
| 9d678c8 | MsgPack | #{iso => 11.2637, read => 2.8620, write => 9.0897} |
| 3d3b04e | JSON_Verbose | #{iso => 34.236, read => 9.724, write => 25.638} |
| c976ce6 | JSON_Verbose | #{iso => 36.613, read => 9.572, write => 27.120} |
| 9d678c8 | JSON_Verbose | #{iso => 33.36954,  read => 8.59574, write => 29.23906} |

Some important timings are that `jsx` decodes in 5.630 ms and `msgpack` decodes in 0.930 ms. These are therefore the minimum timings and the rest is transit-specific overhead of decoding.

Current limitations
--------------------

* Points-in-time before the date 1/1 1970 are not encoded and decoded correctly.

Default type mapping
--------------------

We currently handle the types in the given table with the given mappings.

*Rationale for the mapping*: The problem we face in Erlang w.r.t transit is that we can't really map external data directly into `atom()` types. The reason is the atom-table is limited and an enemy can easily outrun it. Other language implementations are not with this limit, so they will just use keywords as they go along, ignoring all limitations of them in Erlang. Thus, we opt for a solution where the low-level mapping is to map a lot of things into binary types, but tag them as we do so to discriminate them.

We chose to handle a "naked" `binary()` as an UTF-8 string.

### Mapping override

In order to handle data in a neat Erlang-esque way, it is possible to supply a translation table in the decoder direction. This table is used to handle scalar types and map them into other data. The intended use is to support `binary() → atom()` conversion for keywords and symbols. But it also useful for direct decoding of other types.

```erlang
9> transit:write({kw, <<"foo">>}).                                                                        
<<"[\"~#'\",\"~:foo\"]">>
10> transit:read(v(9), #{ format => json, translate_table => fun ({kw, <<"foo">>}) -> foo; (X) -> X end }).
foo
```

We are currently not able to support:

* Link types

| Transit type | Write accepts             | Read returns              |
| ------------ | -------------             | ------------              |
| null         | undefined                 | undefined                 |
| string       | binary()                  | binary()                  |
| boolean      | true, false               | true, false               |
| integer      | integer()                 | integer()                 |
| decimal      | float()                   | float()                   |
| big integer  | integer()                 | integer()                 |
| time         | {timepoint, now()}        | {timepoint, now()         |
| keyword      | {kw, binary()}            | {kw, binary()}
| symbol       | {sym, binary()}        | {sym, binary()}        |
| uri          | {uri, binary()}           | {uri, binary()}        |
| uuid         | {uuid, binary()}                 | {uuid, binary()}                  |
| bytes		   | {binary, binary()}   | {binary, binary()}  |
| special number | nan, infinity, neg_infinity | nan, infinity, neg_infinity |
| array        | list                      | list                      |
| list         | {list, list(transit())}     | {list, list(transit())}     |
| set          | sets, gb\_sets, ordsets   | sets                      |
| map          | map                       | map                       |
