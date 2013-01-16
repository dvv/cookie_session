%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
  {Session, Req2} = cowboy_req:meta(session, Req),
  error_logger:info_report({session_was, Session}),
  Req3 = cowboy_cookie_session:set_session({shiny, new, 'SESSION'}, Req2),
	{ok, Req4} = cowboy_req:reply(200, [], <<"Look at server console">>, Req3),
	{ok, Req4, State}.

terminate(_Req, _State) ->
	ok.
