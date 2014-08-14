.PHONY: test

test:
	rebar compile && rebar get_deps=false eunit ct

default: test
