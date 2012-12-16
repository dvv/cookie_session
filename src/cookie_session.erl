%%
%% @doc Manage sessions in signed encrypted cookies.
%%
-module(cookie_session).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([get/2, set/3, drop/2]).

-define(INFO, error_logger:info_report).
-define(ERROR, error_logger:error_report).

%%
%% @doc Serialize Term, encrypt and sign the result with Secret.
%%

-spec encode(Term :: any(), Secret :: binary()) -> Cipher :: binary().

encode(Term, Secret) ->
  Bin = term_to_binary(Term),
  Enc = encrypt(Bin, Secret),
  {MegaSecs, Secs, _} = erlang:now(),
  Time = list_to_binary(integer_to_list(MegaSecs * 1000000 + Secs)),
  Sig = sign(<<Time/binary, Enc/binary>>, Secret),
  <<Sig/binary, Time/binary, Enc/binary>>.

%%
%% @doc Given a signed encrypted binary, check the signature,
%% uncrypt and deserialize into original Term.
%% Check it timestamp encoded into the data is not older than Ttl.
%%

-spec decode(
    Cipher :: binary(),
    Secret :: binary(),
    Ttl :: non_neg_integer()
  ) -> Term :: any().

decode(<<Sig:32/binary, Time:10/binary, Enc/binary>>, Secret, Ttl) ->
  case sign(<<Time/binary, Enc/binary>>, Secret) of
    % signature ok?
    Sig ->
      Bin = decrypt(Enc, Secret),
      % deserialize
      try binary_to_term(Bin, [safe]) of
        Term ->
          % not yet expired?
          {MegaSecs, Secs, _} = erlang:now(),
          Now = MegaSecs * 1000000 + Secs,
          Expires = list_to_integer(binary_to_list(Time)) + Ttl,
          case Expires > Now of
            true ->
              {ok, Term};
            false ->
              {error, expired}
          end
      catch _:_ ->
          {error, badarg}
      end;
    _ ->
      {error, forged}
  end;

%% N.B. unmatched binaries are forged
decode(Bin, _, _) when is_binary(Bin) ->
  {error, forged}.

%%
%% @doc Get 32-byte SHA1 sum of Data salted with Secret.
%%

-spec sign(binary(), binary()) -> binary().

sign(Data, Secret) ->
  crypto:sha256([Data, Secret]).

%%
%% @doc Encrypt Bin using Secret.
%%

-spec encrypt(binary(), binary()) -> binary().

encrypt(Bin, Secret) ->
  <<Key:16/binary, IV:16/binary>> = crypto:sha256(Secret),
  crypto:aes_cfb_128_encrypt(Key, IV, Bin).

%%
%% @doc Decrypt Bin using Secret.
%%

-spec decrypt(binary(), binary()) -> binary().

decrypt(Bin, Secret) ->
  <<Key:16/binary, IV:16/binary>> = crypto:sha256(Secret),
  crypto:aes_cfb_128_decrypt(Key, IV, Bin).

%%
%% ------------------------------------------------------------------
%% Conversion helpers
%% ------------------------------------------------------------------
%%

encode_base64(Term, Secret) ->
  base64:encode(encode(Term, Secret)).

decode_base64(undefined, _, _) ->
  {error, forged};

decode_base64(Bin, Secret, Ttl) ->
  decode(base64:decode(Bin), Secret, Ttl).

%%
%% ------------------------------------------------------------------
%% Cowboy handler helpers
%% ------------------------------------------------------------------
%%

%%
%% @doc Store term Session into cookie Name secured with Secret with
%% time-to-live MaxAge to cowboy request Req.
%%
%% N.B. undefined Session means drop session.
%%

-spec set(
    Session :: any(),
    { Name :: binary(), Secret :: binary(), MaxAge :: non_neg_integer() },
    Req :: any()
  ) -> { Cookie :: binary(), Req :: any() }.

set(Session, {Name, Secret, MaxAge}, Req) ->
  % serialize session
  Cookie = encode_base64(Session, Secret),
  % N.B. base64 may end with equal signs which are invalid for cookie value
  % so we always quote the value.
  Req2 = cowboy_req:set_resp_cookie(Name, << $", Cookie/binary, $" >>, [
      http_only,
      {max_age, MaxAge}
    ], Req),
  {Cookie, Req2}.

%%
%% @doc Destroy session by writing expired session cookie Name.
%%

-spec drop(
    { Name :: binary(), any(), any() },
    any()
  ) -> Req :: any().

drop({Name, _Secret, _MaxAge}, Req) ->
  % write already expired session cookie
  cowboy_req:set_resp_cookie(Name, <<>>, [
      http_only,
      {max_age, 0}
    ], Req).

%%
%% @doc Restore session from cookie Name secured with Secret with
%% time-to-live MaxAge from cowboy request Req.
%%

-spec get(
    { Name :: binary(), Secret :: binary(), MaxAge :: non_neg_integer() },
    any()
  ) -> { Session :: any(), Cookie :: binary(), Req :: any() }.

get({Name, Secret, MaxAge}, Req) ->
  % get cookie
  {Cookie, Req2} = cowboy_req:cookie(Name, Req),
  % deserialize cookie into session
  % N.B. error results in void session.
  Session = case decode_base64(Cookie, Secret, MaxAge) of
      {ok, Sess} ->
        Sess;
      _ ->
        undefined
    end,
  {Session, Cookie, Req2}.

%%
%% ------------------------------------------------------------------
%% Some unit tests
%% ------------------------------------------------------------------
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

check(Req, Res) ->
  case Req of
    Res ->
      ok;
    Else ->
      ?ERROR({got, Else, expected, Res}),
      Res = Else
  end.

check_not(Req, Res) ->
  case Req of
    Res ->
      ?ERROR({unexpected, Res}),
      throw;
    _Else ->
      ok
  end.

encrypt_test() ->
  Secret = <<"Make It Elegant">>,
  Bin = <<"Transire Benefaciendo">>,
  check(decrypt(encrypt(Bin, Secret), Secret), Bin),
  check_not(decrypt(encrypt(Bin, Secret), <<Secret/binary, "1">>), Bin),
  check_not(decrypt(encrypt(Bin, <<Secret/binary, "1">>), Secret), Bin).

smoke_test() ->
  Term = {a, b, c, [d, "e", <<"foo">>]},
  Secret = <<"TopSecRet">>,
  Enc = encode(Term, Secret),
  % decode encoded term with valid time to live
  check(decode(Enc, Secret, 1), {ok, Term}),
  % expired data
  check(decode(encode(Term, Secret), Secret, 0), {error, expired}),
  % forged data
  check(decode(<<"1">>, Secret, 1), {error, forged}),
  check(decode(<<Enc/binary, "1">>, Secret, 1), {error, forged}).

encode_test() ->
  Term = {a, b, c, [d, "e", <<"foo">>]},
  Secret = <<"TopSecRet">>,
  check(decode_base64(encode_base64(Term, Secret), Secret, 1), {ok, Term}).

-endif.

