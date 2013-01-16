Cookie Session
==============

Library for managing a moderately sized sessions inside secure signed encrypted cookies.

Typical usage
--------------

Take a look at [examples](cookie_session/tree/master/examples).

A typical use case for [Cowboy](/extend/cowboy) Web server:

```erlang
handle(Req, State) ->

  % session params
  SessionOpts = {
      <<"s">>,    % cookie name
      <<"FOO">>,  % cipher secret
      1000,       % session time-to-live in seconds, older sessions are expired
      <<"/">>     % cookie path
    },

  % get previous session
  {Session, Req2} = cowboy_cookie_session:get_session(SessionOpts, Req),

  % do the job
  % ...
  {Status, Headers, Body, Req3} = {200, [], <<"OK">>, Req2},

  % set new session
  Req4 = case KeepSession of
      true ->
        Session2 = {foo, bar},
        cowboy_cookie_session:set_session(Session2, SessionOpts, Req3);
      false ->
        cowboy_cookie_session:set_session(undefined, SessionOpts, Req3)
    end,

  % respond
  {ok, Req5} = cowboy_req:reply(Status, Headers, Body, Req4),
  {ok, Req5, State}.
```

Cowboy middleware
--------------
A [Cowboy](/extend/cowboy) middleware is also provided [here](cookie_session/blob/master/src/cowboy_cookie_session.erl).

Protocol options passed to `cowboy:start_http/4` should contain:
```erlang

  ...

  % middleware
  {middlewares, [
      cowboy_router,
      cowboy_cookie_session,
      cowboy_handler]},

  % environment
  {env, [

    % session parameters
    {session_opts, {
        <<"sid">>,       % cookie name
        <<"tOpcekpet">>, % encryption secret
        1000,            % cookie time-to-live in seconds
        <<"/">>}},       % cookie path
    ...
```

Then in the handler:

```erlang
init(_Transport, Req, Opts) ->

  % get previous session
  {session, Session, SessionOpts} = lists:keyfind(session, 1, Opts),
  {ok, Req, {Session, SessionOpts}}.

handle(Req, {Session, SessionOpts} = State) ->

  % do the job
  % ...
  {Status, Headers, Body, Req2} = {200, [], <<"OK">>, Req},

  % mangle the session
  Session2 = [{foo, bar} | Session],

  % set new session
  Req3 = cowboy_cookie_session:set_session(Session2, SessionOpts, Req2},

  % respond
  {ok, Req4} = cowboy_req:reply(Status, Headers, Body, Req3),
  {ok, Req4, State}.
```

Elli middleware
--------------
An [Elli](/knutin/elli) middleware is also provided [here](cookie_session/blob/master/src/elli_cookie_session.erl).
Note, that Elli doesn't provide cookie management means per se, and you'll need to depend on [elli_cookie](/drfloob/elli_cookie).

```erlang
% session params
SessionOpts = {
    <<"s">>,    % cookie name
    <<"FOO">>,  % cipher secret
    1000,       % session time-to-live in seconds, older sessions are expired
    <<"/">>     % cookie path
  },

% specify middleware layers
Middleware = [{mods, [
  % cookie session
  {elli_cookie_session, SessionOpts},
  ...
]}],

% start server
elli:start_link([
  {callback, elli_middleware},
  {callback_args, Middleware},
  ...
]).
```

Then in the handler:

```erlang
handle(Req = #req{args = Args}, _Args) ->

  % get previous session
  {session, Session} = lists:keyfind(session, 1, Args),

  % do the job
  % ...
  {Status, Headers, Body} = {200, [], <<"OK">>},

  % mangle the session
  Session2 = [{foo, bar} | Session],

  % set new session
  % NB: special return for honoring session
  {with_session, Status, Headers, Body, Session2}.
```

License
-------

Copyright (c) 2013 Vladimir Dronnikov <dronnikov@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
