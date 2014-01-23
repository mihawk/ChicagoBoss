-module(wamp_procedure).
-export([authreq/2]).
-export([auth/1]).

auth_signature(AuthChallenge, undefined) ->
	auth_signature(AuthChallenge, "");
auth_signature(AuthChallenge, AuthSecret) ->
 <<Mac:160/integer>> = hmac:hmac(AuthSecret, AuthChallenge),
 lists:flatten(io_lib:format("~40.16.0b", [Mac])).

authreq(null, null) ->
  authreq("", "");
authreq(Username, Password) ->
  {ok, list_to_binary(auth_signature(Username, Password))}.

auth(null) ->
  auth("");
auth(_Param) -> 
        %%error_logger:info_msg("~p auth ~p", [?MODULE, _Param]), 
	{ok, true}.
