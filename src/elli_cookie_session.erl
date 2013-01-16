-module(elli_cookie_session).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(elli_handler).
-export([preprocess/2, postprocess/3]).

-include_lib("elli/include/elli.hrl").

preprocess(Req = #req{args = Args}, CookieOpts) ->
  % get session
  Session = get_session(CookieOpts, Req),
  % attach session to request
  Req#req{args = [{session, Session} | Args]}.

postprocess(_Req, {with_session, ResponseCode, Headers, Body, Session}, CookieOpts) ->
  error_logger:info_report({session_post, Session, CookieOpts}),
  Cookie = set_session(Session, CookieOpts),
  {ResponseCode, [Cookie | Headers], Body};

postprocess(Req, Res, Args) ->
  Res.

%%
%% Get session from cookie.
%%

-spec get_session(Opts :: tuple(), Req :: binary()) ->
    {Session :: term(), Req :: binary()}.

get_session({Name, Secret, MaxAge, _Path}, Req) ->
  % get cookie
  case elli_cookie:parse(Req) of
    CookieList when is_list(CookieList) ->
      case lists:keyfind(Name, 1, CookieList) of
        false ->
          undefined;
        {Name, Cookie} ->
          % deserialize session
          Value = binary:replace(Cookie, <<"_">>, <<"=">>, [global]),
          case termit:decode_base64(Value, Secret, MaxAge) of
            {error, _Reason} ->
              undefined;
            {ok, undefined} ->
              undefined;
            {ok, Sess} ->
              Sess
          end
      end;
    _ ->
      undefined
  end.

%%
%% Set session to cookie.
%%

-spec set_session(Session :: term(), Opts :: tuple()) ->
    SetCookie :: {binary(), binary()}.

set_session(undefined, {Name, _Secret, _MaxAge, _Path}) ->
  elli_cookie:delete(Name);

set_session(Session, {Name, Secret, MaxAge, Path}) ->
  Value = termit:encode_base64(Session, Secret),
  SafeValue = binary:replace(Value, <<"=">>, <<"_">>, [global]),
  error_logger:info_report({set_session, SafeValue}),
  elli_cookie:new(Name, SafeValue, [
      elli_cookie:http_only(),
      elli_cookie:path(Path),
      elli_cookie:expires({MaxAge, seconds})]).
