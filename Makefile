deps:
	rebar get-deps

compile:
	rebar compile

run: compile
	sh start.sh

clean:
	rebar clean
	rm -fr ebin

check:
	rebar eunit skip_deps=true

dist: deps compile
	echo TODO

.PHONY: all deps compile check run clean dist
.SILENT:

