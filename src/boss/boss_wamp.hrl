%%%-------------------------------------------------------------------
%%% @author mihawk <mihawk@abc>
%%% @copyright (C) 2014, mihawk
%%% @doc
%%% @end
%%% Created : 16 Jan 2014 by mihawk <mihawk@abc>
%%%-------------------------------------------------------------------

-define(WAMP_PROTOCOL_VERSION, 1).
-define(__VERSION__, "0.1.0").
-define(WAMP_WELCOME,     0).
-define(WAMP_PREFIX,      1).
-define(WAMP_CALL,        2).
-define(WAMP_CALLRESULT,  3).
-define(WAMP_CALLERROR,   4).
-define(WAMP_SUBSCRIBE,   5).
-define(WAMP_UNSUBSCRIBE, 6).
-define(WAMP_PUBLISH,     7).
-define(WAMP_EVENT,       8).

-define(TAB, boss_wamp).

-record(state, {
          wamp_directory = undefined :: dict()
         }).

-record(frame_ctx, {
                  service_url = undefined ::string(),
                  session_id  = undefined ::string(),
                  websocket_id= undefined ::pid(),
                  request     = undefined
                 }).  

-record(wamp_event, {
                    event      = undefined ::any(),
                    exclude_me = false     ::boolean(),
                    exclude    = []        ::list(),
                    eligible   = []        ::list()
                 }).  

-define(json_encode(X), jsx:encode(X)).
-define(json_decode(X), jsx:decode(X)).

