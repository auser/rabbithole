PACKAGE_NAME = rabbithole
PACKAGE_VERSION = 0.1

.PHONY: compile rel test deps

all: deps compile

compile:
	@./rebar compile

deps:
	@./bootstrap.sh
	@./rebar get-deps

clean:
	@./rebar clean

rel: all
	@(cp -Rf etc/app_templates rel/overlay/etc)
	@(make rel_erlang)

rel_erlang:
	@./rebar generate force=1

test: compile
	./rebar skip_deps=true eunit
