-module(cookie_session_cowboy).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-define(INFO, error_logger:info_report).
-define(ERROR, error_logger:error_report).

%%
%% ------------------------------------------------------------------
%% cowboy HTTP handler API
%% ------------------------------------------------------------------
%%
%-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

%%
%% ------------------------------------------------------------------
%% cowboy HTTP handler API
%% ------------------------------------------------------------------
%%

-record(state, {
    handler,
    session
  }).

%% Opts :: [{handler, Module} | {handler, {M, F}} | {handler, function()}]
init(_Transport, Req, Opts) ->
  % handler
  {handler, Handler} = lists:keyfind(handler, 1, Opts),
  HandlerFun = case Handler of
      Fun when is_function(Fun) -> Fun;
      {M, F} -> fun M:F/3;
      Mod -> fun Mod:handle/3
    end,
  % session
  Req2 = case proplists:get_value(session, Opts) of
      {SessionManager, SessionOpts} ->
        % get session
        {Session, Cookie, Req3} = SessionManager:get(SessionOpts, Req),
        ?INFO({insess, Session, Cookie}),
        Req4 = cowboy_req:set_meta(session, Session, Req3),
        cowboy_req:set_meta(session_setter, fun (Session2, Req5) ->
          {Cookie2, Req6} = SessionManager:set(Session2, SessionOpts, Req5),
          ?INFO({outsess, Session2, Cookie2}),
          Req6
        end, Req4);
      _ ->
        cowboy_req:set_meta(session_setter, fun (_Session2, Req5) ->
          Req5
        end, Req)
    end,
  %
  {ok, Req2, #state{handler = HandlerFun}}.

terminate(_Req, _State) ->
  ok.

handle(Req, State = #state{handler = Handler}) ->

  % route request
  {Method, Req2} = cowboy_req:method(Req),
  {Path, Req3} = cowboy_req:path_info(Req2),
  {Status, Headers, Body, Req4} = Handler(Method, Path, Req3),

  % set session
  {Session2, Req5} = cowboy_req:meta(session, Req4),
  {Setter, Req6} = cowboy_req:meta(session_setter, Req5),
  Req7 = Setter(Session2, Req6),

  % respond
  {ok, Req8} = cowboy_req:reply(Status, Headers, Body, Req7),
  {ok, Req8, State}.
