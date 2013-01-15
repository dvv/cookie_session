Cookie Session
==============

Library for managing a moderately sized sessions inside secure signed encrypted cookies.

Typical usage
--------------

A typical use case in case of [cowboy](/extend/cowboy) Web server:

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
A cowboy middleware is also provided [here](cookie_session/blob/master/src/cowboy_cookie_session.erl).

Protocol options passed to `cowboy:start_http/4` should contain:
```erlang

  ...

  % middleware
  {middlewares, [
      cowboy_router,
      cowboy_cookie_session, % <- middlewares below will have session meta in Req
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
handle(Req, State) ->

  % get previous session
  {Session, Req2} = cowboy_req:meta(session, Req),

  % do the job
  % ...
  {Status, Headers, Body, Req3} = {200, [], <<"OK">>, Req2},

  % set new session
  Session2 = {foo, bar},
  Req4 = cowboy_cookie_session:set_session(Session2, Req3);

  % respond
  {ok, Req5} = cowboy_req:reply(Status, Headers, Body, Req4),
  {ok, Req5, State}.
```

A cowboy middleware is also provided [here](cookie_session/blob/master/src/cowboy_cookie_session.erl).

[License](cookie_session/blob/master/LICENSE.txt)
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
