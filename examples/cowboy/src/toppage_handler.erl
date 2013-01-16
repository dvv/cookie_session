%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, Opts) ->

  % get previous session
  {session, Session, CookieOpts} = lists:keyfind(session, 1, Opts),
  error_logger:info_report({session_was, Session}),
  {ok, Req, {Session, CookieOpts}}.

handle(Req, {Session, CookieOpts} = State) ->

  Session2 = [{foo, bar} | Session],

  % set new session
  Req2 = cowboy_cookie_session:set_session(Session2, CookieOpts, Req),

  % respond
	{ok, Req3} = cowboy_req:reply(200, [], <<"Look at server console">>, Req2),
  {ok, Req3, State}.

terminate(_Req, _State) ->
	ok.
