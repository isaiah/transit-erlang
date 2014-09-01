[![Build
Status](https://travis-ci.org/isaiah/transit-erlang.svg)](https://travis-ci.org/isaiah/transit-erlang)

transit-erlang
==============
[transit-format](https://github.com/cognitect/transit-format) implementation in Erlang.

Test and developed on Erlang/OTP R17.

Usage
-----

```shell
rebar get-deps compile
erl -pa ebin deps/*/ebin
```

```erlang
A = transit:write(#{"a" => "b", 3 => 4}, [{format, json}]).
%% => <<"[\"^ \",\"a\",\"b\",3,4]">>
transit:read(A, [{format, json}]).
%% => [{"a", "b"}, {3, 4}]

%%% JSON Verbose mode
transit:write(#{"a" => "b", 3 => 4}, [{format, json_verbose}]).
%% => <<"{\"~i3\":4,\"a\":\"b\"}">>

%%% msgpack
transit:write(#{"a" => "b", 3 => 4}, [{format, msgpack}]).
%% => <<149,162,94,32,161,97,161,98,163,126,105,51,4>>
```

Default type mapping
--------------------

|Transit type|Write accepts|Read returns|
|------------|-------------|------------|
|null|undefined|undefined|
|string|binary()|binary()|
|boolean|true, false|true, false|
|integer|integer()|integer()|
|decimal|float()|float()|
|keyword|atom()|atom()|
|symbol|transit\_types:symbol()|transit\_types:symbol()|
|big decimal|float()|float()|
|big integer|integer()|integer()|
|time|transit\_types:datetime()|transit\_types:datetime()|
|uri|transit\_types.URI|transit\_types.URI|
|uuid|uuid.UUID|uuid.UUID|
|array|list|list|
|list|transit\_types:list()|transit\_types:list()|
|set|sets, gb_sets, ordsets|sets|
|map|proplists, map|map, proplists|
