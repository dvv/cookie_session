-module(cowboy_cookie_session).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2, get_session/2, set_session/3, set_session/2]).

execute(Req, Env) ->

  % session_opts tuple is required in Env
  {session_opts, CookieOpts} =
      lists:keyfind(session_opts, 1, Env),

  % get session
  {Session, Req2} = get_session(CookieOpts, Req),

  % set session setter
  Req3 = cowboy_req:set_meta(session_setter, fun (NewSession, Req0) ->
      set_session(NewSession, CookieOpts, Req0)
    end, Req2),

  % attach session to request
  Req4 = cowboy_req:set_meta(session, Session, Req3),
  {ok, Req4, Env}.

%%
%% Get session from cookie.
%%

-spec get_session(Opts :: tuple(), Req :: binary()) ->
    {Session :: term(), Req :: binary()}.

get_session({Name, Secret, MaxAge, _Path}, Req) ->
  % get cookie
  {Cookie, Req2} = cowboy_req:cookie(Name, Req),
  % deserialize session
  Session = case termit:decode_base64(Cookie, Secret, MaxAge) of
    {error, _Reason} ->
      undefined;
    {ok, undefined} ->
      undefined;
    {ok, Sess} ->
      Sess
  end,
  {Session, Req2}.

%%
%% Set session to cookie, options explicitly passed.
%%

-spec set_session(Session :: term(), Opts :: tuple(), Req :: binary()) ->
    Req2 :: binary().

set_session(undefined, {Name, _Secret, _MaxAge, Path}, Req) ->
  % write already expired cookie
  cowboy_req:set_resp_cookie(Name, <<>>,
      [http_only, {max_age, 0}, {path, Path}], Req);

set_session(Session, {Name, Secret, MaxAge, Path}, Req) ->
  Cookie = termit:encode_base64(Session, Secret),
  cowboy_req:set_resp_cookie(Name, Cookie,
      [http_only, {max_age, MaxAge}, {path, Path}], Req).

%%
%% Set session to cookie, applies session setter bound to request.
%%

-spec set_session(Session :: term(), Req :: binary()) ->
    Req2 :: binary().

set_session(Session, Req) ->
  {Setter, Req2} = cowboy_req:meta(session_setter, Req),
  Setter(Session, Req2).
