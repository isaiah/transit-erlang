.DEFAULT_GOAL := test

.PHONY: deps
deps:
	rebar get-deps

.PHONY: compile
compile:
	rebar compile

.PHONY: test
test: compile
	rebar get_deps=false eunit ct

# EQC
eqc-ci: deps compile
	erlc -o ebin test/*_eqc.erl

# Deps directory.

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DEPS))
ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))

# Dialyzer.

PLT_APPS = sasl
DIALYZER_PLT ?= $(CURDIR)/.$(PROJECT).plt
export DIALYZER_PLT

PLT_APPS ?=
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions \
	-Wunmatched_returns # -Wunderspecs

.PHONY: build-plt
build-plt: compile
	@dialyzer --build_plt --apps erts kernel stdlib $(PLT_APPS) $(ALL_DEPS_DIRS)

dialyze:
	@dialyzer --src src --no_native $(DIALYZER_OPTS)

default: test

clean:
	rebar clean
