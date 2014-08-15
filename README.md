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
%%% In erl shell
> A = transit:write(#{"a" => "b", 3 => 4}, [{format, json}]).
%=> <<"[\"^ \",\"a\",\"b\",3,4]">>
> transit:read(A, [{format, json}]).
%=> [{"a", "b"}, {3, 4}]
```

JSON verbose mode

```erlang
> A = transit:write(#{"a" => "b", 3 => 4}, [{format, json_verbose}]).
%=> <<"{\"~i3\":4,\"a\":\"b\"}">>
```
