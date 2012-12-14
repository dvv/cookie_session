Cookie Session
==============

Library to manage a moderate sized sessions inside secure signed encrypted cookies.

Usage
--------------

A typical use case in case of [cowboy](/extend/cowboy) Web server:

```erlang
handle(Req, State) ->

  % session params
  SessionOpts = {
      <<"s">>,    % cookie name
      <<"FOO">>,  % cipher secret
      1000        % session time-to-live in seconds, older sessions are expired
    },

  % get previous session
  {Session, Cookie, Req2} = cookie_session:get(SessionOpts, Req),

  % do the job
  % ...
  {Status, Headers, Body, Req3} = {200, [], <<"OK">>, Req2},

  % set new session
  {Cookie2, Req4} = case KeepSession of
      true ->
        Session2 = {foo, bar},
        cookie_session:set(Session2, SessionOpts, Req3);
      false ->
        cookie_session:drop(SessionOpts, Req3)
    end,

  % respond
  {ok, Req5} = cowboy_req:reply(Status, Headers, Body, Req4),
  {ok, Req5, State}.
```

[License](cookie_session/blob/master/LICENSE.txt)
-------

Copyright (c) 2012 Vladimir Dronnikov <dronnikov@gmail.com>

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
