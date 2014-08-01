[![Build
Status](https://travis-ci.org/isaiah/transit-erlang.svg)](https://travis-ci.org/isaiah/transit-erlang)

transit-erlang
==============

Test and developed on erlang R17.

Usage
-----

```shell
rebar compile
erl -pa ebin
```

```erlang
%%% In erl shell
> transit_writer:start()
> transit_writer:write(#{"a" => "b", 3 => 4})
=> <<"[\"^ \",\"a\",\"b\",3,4]">>
```
