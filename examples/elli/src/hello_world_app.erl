%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  Middleware = [{mods, [
    % cookie session
    {elli_cookie_session, {<<"s">>, <<"tOpcekpet">>, 1000, <<"/">>}},
    {toppage_handler, []}
  ]}],
  % start server
  elli:start_link([
    {callback, elli_middleware},
    {callback_args, Middleware},
    {port, 8080},
    {ip, {127, 0, 0, 1}}
  ]),
	hello_world_sup:start_link().

stop(_State) ->
	ok.
