#!/bin/sh
erl -pa ebin deps/*/ebin -s hello_world \
	-eval "io:format(\"curl -v -b .cookie -c .cookie http://localhost:8080~n\")."
