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
> transit:start().
%=> ok.
> A = transit:write(#{"a" => "b", 3 => 4}).
%=> <<"[\"^ \",\"a\",\"b\",3,4]">>
> transit:read(A).
%=> [{"a", "b"}, {3, 4}]
```

JSON verbose mode

```shell```
erl -pa ebin deps/*/ebin -transit format json_verbose
```

```erlang
> transit:start().
%=> ok.
> A = transit:write(#{"a" => "b", 3 => 4}).
%=> <<"{\"~i3\":4,\"a\":\"b\"}">>
```
