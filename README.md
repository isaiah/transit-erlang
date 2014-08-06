[![Build
Status](https://travis-ci.org/isaiah/transit-erlang.svg)](https://travis-ci.org/isaiah/transit-erlang)

transit-erlang
==============

Test and developed on Erlang/OTP R17.

Usage
-----

```shell
rebar get-deps compile
erl -pa ebin deps/*/ebin
```

```erlang
%%% In erl shell
> transit_writer:start().
> transit_writer:write(#{"a" => "b", 3 => 4}).
=> <<"[\"^ \",\"a\",\"b\",3,4]">>
> transit_reader:start().
> transit_reader:read(<<"[\"~#'\",\"foo\"]">>).
=> <<"foo">>
```
