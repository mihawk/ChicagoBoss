-module(boss_mochicow_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, loop/1, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, _Opts) ->
    case cowboy_req:header(<<"upgrade">>, Req) of
	{undefined, _Req2} -> {upgrade, protocol, mochicow_upgrade};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket};
	{<<"Websocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket}
    end.

-record(state, {websocket_id, session_id, service_url, protocol, wamp_id}).

loop(Req) ->
    boss_web_controller_handle_request:handle_request(Req, 
						      mochiweb_request_bridge, 
						      mochiweb_response_bridge).

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, _Opts) ->
    SessionKey = boss_env:get_env(session_key, "_boss_session"),
    {ServiceUrl, Req1} = cowboy_req:path(Req),
    {SessionId, Req2}  = cowboy_req:cookie(list_to_binary(SessionKey), Req1),
    {Protocol, Req3} = cowboy_req:header(<<"sec-websocket-protocol">>, Req2),
    State= #state{websocket_id=self(), 
		  session_id=SessionId,
		  service_url=ServiceUrl,
                  protocol=Protocol},
    handle_protocol(Req3, State).


websocket_handle({text, Msg}, Req, 
                 #state{websocket_id=WebsocketId, 
                        session_id=SessionId, 
                        wamp_id=WampId, 
                        service_url=ServiceUrl,
                        protocol= <<"wamp">>} = State) ->
    boss_wamp:incoming(ServiceUrl, WebsocketId, Req, SessionId, Msg, WampId),
    {ok, Req, State, hibernate};

websocket_handle({text, Msg}, Req, State) ->
    #state{websocket_id=WebsocketId, 
	   session_id=SessionId, 
	   service_url=ServiceUrl } = State,
    boss_websocket_router:incoming(ServiceUrl, WebsocketId, Req, SessionId, Msg),
    {ok, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(Reason, Req,
                    #state{websocket_id=WebsocketId, 
                           session_id=SessionId, 
                           wamp_id=WampId,
                           service_url=ServiceUrl,                           
                           protocol = <<"wamp">>}) ->
    boss_wamp:close(Reason, ServiceUrl, WebsocketId, Req, SessionId, WampId),
    ok;

websocket_terminate(Reason, Req, State) ->
    #state{websocket_id=WebsocketId, 
	   session_id=SessionId, 
	   service_url=ServiceUrl } = State,
    boss_websocket_router:close(Reason, ServiceUrl, WebsocketId, Req, SessionId),
    ok.

handle_protocol(Req, #state{websocket_id=WebsocketId, 
                            session_id=SessionId,
                            service_url=ServiceUrl,
                            protocol= <<"wamp">> }=State) ->
    {WampId, Req2} = boss_wamp:welcome(ServiceUrl, WebsocketId, Req, SessionId),
    config_websocket(Req2, State#state{wamp_id=WampId});
handle_protocol(Req, #state{websocket_id=WebsocketId, 
                            session_id=SessionId,
                            service_url=ServiceUrl}=State) ->
    boss_websocket_router:join(ServiceUrl, WebsocketId, Req, SessionId),
    config_websocket(Req, State).

config_websocket(Req, State) ->
    case boss_env:get_env(websocket_timeout, undefined) of
        undefined ->
            {ok, Req, State, hibernate};
        Timeout ->
            {ok, Req, State, Timeout, hibernate}
    end.
    
