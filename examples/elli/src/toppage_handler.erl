%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-include_lib("../deps/elli/include/elli.hrl").
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).

handle(Req = #req{args = Args}, _Args) ->
  {session, Session} = lists:keyfind(session, 1, Args),
  error_logger:info_report({session_was, Req, Args}),
  % special return for honoring session
  {with_session, 200, [], <<"Look at server console">>, [{foo, bar} | Session]}.

handle_event(_Event, _Data, _Args) ->
  ok.
