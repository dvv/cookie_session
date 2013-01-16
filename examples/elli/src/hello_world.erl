%% Feel free to use, reuse and abuse the code in this file.

-module(hello_world).

%% API.
-export([start/0]).

%% API.

start() ->
	ok = application:start(elli),
	ok = application:start(hello_world).
