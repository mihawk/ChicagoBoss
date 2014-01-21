%%%-------------------------------------------------------------------
%%% @author mihawk <mihawk@abc>
%%% @copyright (C) 2014, mihawk
%%% @doc
%%% @end
%%% Created : 16 Jan 2014 by mihawk <mihawk@abc>
%%%-------------------------------------------------------------------

-module(boss_wamp_handle).

-export([process_frame/3]).

-export([start_ws_agent_sup/4]).
-export([ws_agent/4]).

-include("boss_wamp.hrl").

-spec process_frame(nonempty_maybe_improper_list(),_,#state{wamp_directory::dict()}) -> {'ok',#state{wamp_directory::dict()}}.
-spec call(_,atom(),_,_) -> {ok, any()} | {error, any()} | {error, any(), any()}.
-spec apply_function(atom(),atom(),[any()],#frame_ctx{service_url::string(),session_id::string(),websocket_id::pid()}) -> any().
-spec maybe_pmod(atom(),atom(),[any()],#frame_ctx{service_url::string(),session_id::string(),websocket_id::pid()}) -> any().
-spec get_MF_from_uri(binary(),dict()) -> {atom(),atom()}.
-spec clean_url(binary()) -> [{'host' | 'path' | 'postfix' | 'prefix' | 'protocol' | 'uri','http' | 'https' | 'undefined' | binary() | [{_,_},...]},...].
-spec split_url(binary(),'undefined' | binary()) -> [{'host',binary() | [{_,_},...]} | {'path',binary()} | {'postfix','undefined' | binary()} | {'prefix',binary()} | {'protocol','http' | 'https'},...].
-spec maybe_uri(binary()) -> [{'postfix','undefined' | binary()} | {'prefix',binary()},...].
-spec split_host(80 | 443,binary()) -> {binary() | [{'hostname',binary()} | {'port',integer()},...],binary()}.
-spec start_ws_agent_sup(_,_,_,#frame_ctx{service_url::string(),session_id::string(),websocket_id::pid()}) -> {'ok','normal' | 'shutdown' | 'unsubscribe' | 'wsclosed'}.
-spec loop_receive_ws_agent_sup(pid(),_,_,_,#frame_ctx{service_url::string(),session_id::string(),websocket_id::pid()}) -> {'ok','normal' | 'shutdown' | 'unsubscribe' | 'wsclosed'}.
-spec ws_agent(_,_,_,#frame_ctx{service_url::string(),session_id::string(),websocket_id::pid()}) -> no_return().
-spec wamp_publish(_,_) -> any().
-spec wamp_publish1([any()],#frame_ctx{service_url::string(),session_id::string(),websocket_id::pid()}) -> any().
-spec wamp_publish1(_,#wamp_event{exclude_me::boolean(),exclude::[any()],eligible::[any()]},#frame_ctx{service_url::string(),session_id::string(),websocket_id::pid()}) -> any().
-spec maybe_notify(_,[#wamp_event{exclude_me::boolean(),exclude::[any()],eligible::[any()]}],pid(),string()) -> [any()].
-spec maybe_notify1(_,#wamp_event{exclude_me::boolean(),exclude::[any()],eligible::[any()]},pid(),string()) -> any().
-spec notify(atom() | binary() | [any()] | number() | {_,_},atom() | binary() | [any()] | number() | {_,_},pid()) -> {'text',binary()}.

process_frame([?WAMP_PREFIX, Prefix, Url], FrameCtx, State) ->
    CleanUrl = clean_url(Url),
    Path = proplists:get_value(path, CleanUrl),
    Module = case dict:find(Path, State#state.wamp_directory) of 
                 {ok, Name } -> Name;
                 _ -> undefined
             end,
    boss_wamp:insert_prefix(Prefix, Module, FrameCtx#frame_ctx.websocket_id),    
    {ok, State};

process_frame([?WAMP_CALL, CallId, Uri | Args], FrameCtx, State) ->
    Dirs = State#state.wamp_directory,
    {Mod, Fun} = get_MF_from_uri(Uri, Dirs),
    lager:debug("Wamp CALL {~p, ~p, ~p}",[Mod, Fun, Args]),
    Reply = call(Mod, Fun, Args, FrameCtx),    
    Addresse = FrameCtx#frame_ctx.websocket_id,
    ReplyJson = case Reply of 
                    {ok, Result} ->
                        ?json_encode([?WAMP_CALLRESULT, CallId, Result]);
                    {error, Reason} ->
                        ?json_encode([?WAMP_CALLERROR, CallId, Reason]);                
                    {error, Reason, Detail} ->
                        ?json_encode([?WAMP_CALLERROR, CallId, Reason, Detail])
                end,    
    Addresse ! {text, ReplyJson},
    {ok, State};

process_frame([?WAMP_SUBSCRIBE, TopicUri], FrameCtx, State) ->
    Dirs = State#state.wamp_directory,
    {Mod, _Fun} = get_MF_from_uri(TopicUri, Dirs),
    lager:debug("Wamp SUBSCRIBE call {~p, ~p, ~p}",[Mod, subscribe, TopicUri]),
    case call(Mod, subscribe, [TopicUri], FrameCtx) of
        {ok, {Topic, Since}} ->
            lager:debug("Wamp SUBSCRIBE return {topic:~p, since:~p}",[Topic, Since]),
            %% process_flag(trap_exit, true), %%??
            spawn(?MODULE, start_agent, [Mod, Topic, Since, FrameCtx]);
        Err ->
            lager:debug("process frame subscribe error ~p", [Err]),            
            Err
    end,
    {ok, State};

process_frame([?WAMP_UNSUBSCRIBE, TopicUri], #frame_ctx{session_id=SessionId}=FrameCtx, State) ->    
    Dirs = State#state.wamp_directory,
    {Mod, _Fun} = get_MF_from_uri(TopicUri, Dirs),    
    {ok, Topic} = call(Mod, unsubscribe, [TopicUri], FrameCtx),    
    Agent = boss_wamp:lookup_agent(SessionId, Topic),
    lager:debug("Wamp UNSUBSCRIBE agent ~p "
                "for sessionId ~p on topic ~p~n", 
                [Agent, SessionId, Topic]),
    Agent ! shutdown,
    boss_wamp:delete_agent(SessionId, Topic),
    {ok, State};

process_frame([?WAMP_PUBLISH, TopicUri, Event | Args], FrameCtx, State) ->
    Dirs = State#state.wamp_directory,
    {Mod, _Fun} = get_MF_from_uri(TopicUri, Dirs),
    Res =  call(Mod, publish, [TopicUri, Event] ++ Args , FrameCtx),        
    wamp_publish(Res, FrameCtx),
    {ok, State}.

call(_Mod, undefined, _Args, _FrameCtx) ->   {error, <<"undefined function">>};
call(undefined, _Fun, _Args, _FrameCtx) ->   {error, <<"undefined module">>};
call(Module, Function, Args, FrameCtx) when is_binary(Module)->
    call(list_to_atom(binary_to_list(Module)), Function, Args, FrameCtx);
call(Module, Function, Args, FrameCtx) when is_atom(Module),
                                       is_atom(Function)->
    apply_function(Module, Function, Args, FrameCtx).

apply_function(Module, Function, Args, FrameCtx) when is_atom(Module) ->
    maybe_pmod(Module, Function, Args, FrameCtx).

maybe_pmod(Mod, Fun, Args, #frame_ctx{request=Req, session_id=SessionID}=FrameCtx) ->
    case proplists:get_value(new, Mod:module_info(exports), undefined) of
        undefined ->
            erlang:apply(Mod, Fun, Args);
        N ->
            Instance = case N of
                           1 -> Mod:new(Req);
                           2 -> Mod:new(Req, SessionID);
                           3 -> Mod:new(Req, SessionID, FrameCtx)
                       end,
            erlang:apply(Instance, Fun, Args)
    end.

get_MF_from_uri(Url, Dirs) ->
    CleanUrl = clean_url(Url),
    Prefix = proplists:get_value(prefix, CleanUrl),
    case Prefix of 
        undefined ->
            Uri = proplists:get_value(uri, CleanUrl),
            Module = case dict:find(Uri, Dirs) of 
                         {ok, Name } -> Name;
                         _ -> undefined
                     end,
            Fun = proplists:get_value(postfix, CleanUrl),
            {Module, list_to_atom(binary_to_list(Fun))};

        Prefix ->
            Module = boss_wamp:lookup_prefix(Prefix, 2),
            Fun = proplists:get_value(postfix, CleanUrl),
            {Module, list_to_atom(binary_to_list(Fun))}
    end.

clean_url(Url) ->
    case binary:match(Url, <<"#">>) of
        nomatch ->
            [{uri, Url}|split_url(Url, undefined)];
        {Pos, _} ->
            << BaseUrl:Pos/binary, _:8, Function/bits >> = Url,
            case Function of
                <<>> ->
                    [{uri, BaseUrl}|split_url(BaseUrl, undefined)];
                 _ ->
                    [{uri, BaseUrl}|split_url(BaseUrl, Function)]
            end
    end.

split_url(<<"http://", Rest/bits>>, Fct) ->
    {Host, Url} = split_host(80, Rest),
    Path = Url,
    [{protocol, http}, {host, Host} ,{path, Path}, {postfix, Fct}];
split_url(<<"https://", Rest/bits>>, Fct) ->
    {Host, Url} = split_host(443, Rest),
    Path = Url,
    [{protocol, https}, {host, Host} ,{path, Path}, {postfix, Fct}];
split_url(Url,_) -> 
    maybe_uri(Url).

maybe_uri(Uri) ->
    case binary:match(Uri, <<":">>) of
        nomatch ->
            [{prefix, Uri},{postfix, undefined}];
        {Pos, _} ->
            << Prefix:Pos/binary, _:8, Postfix/bits >> = Uri,
            [{prefix , Prefix}, {postfix, Postfix}]
    end.
    
split_host(DefaultPort, Url) ->
    case binary:match(Url, <<"/">>) of
        nomatch ->
            {Url, <<>>};
        {Pos, _} ->
            << Segment:Pos/binary, _:8, Rest/bits >> = Url,
            case binary:match(Segment, <<":">>) of
                nomatch ->
                    { [{hostname, Segment}, {port, DefaultPort}], Rest };
                {P, _} ->
                    << Hostname:P/binary, _:8, Port/bits >> = Segment,
                    { [{hostname, Hostname}, {port, binary_to_integer(Port)}], Rest }
            end
    end.


%% boss_mq / tinymq use only string as topic/channel
start_ws_agent_sup(Module, Topic, Since, ReqCtx) when is_binary(Topic) ->
    start_ws_agent_sup(Module, binary_to_list(Topic), Since, ReqCtx);

%% websocket agent supervisor
%% todo: rework, maybe one single sup for all ws_agent instead
start_ws_agent_sup(Module, Topic, Since, FrameCtx) ->
    Agent = spawn_link(?MODULE, ws_agent, [Module, Topic, Since, FrameCtx]),
    lager:info("~p start ws agent ~p on topic ~p~n", [self(), Agent, Topic]),
    boss_wamp:insert_agent(Agent, FrameCtx#frame_ctx.session_id, Topic),    
    loop_receive_ws_agent_sup(Agent, Module, Since, Topic, FrameCtx ).


loop_receive_ws_agent_sup(Agent, Module, Since, Topic, 
                          #frame_ctx{session_id=SessionId}=FrameCtx) ->    
    receive 
        {'EXIT', Pid, normal} -> % 
            boss_wamp:delete_agent(SessionId, Topic),
            lager:debug("ws_agent exit for reason {unsubscribe} "
                        "on topic ~p "
                        "for sessionId ~p, no action taken"
                       ,[Pid, Topic, SessionId]),
            {ok, normal};
        {'EXIT', Pid, unsubscribe} -> % receive frame unsubscribe
            boss_wamp:delete_agent(SessionId, Topic),
            lager:debug("ws_agent exit for reason {unsubscribe} "
                        "on topic ~p "
                        "for sessionId ~p, no action taken"
                       ,[Pid, Topic, SessionId]),
            {ok, unsubscribe};
        {'EXIT', Pid, wsclosed} -> % ws closed
            boss_wamp:delete_agent(SessionId, Topic),
            lager:debug("ws_agent exit for reason {wsclosed} "
                        "on topic ~p "
                        "for sessionId ~p, no action taken"
                       ,[Pid, Topic, SessionId]),
            {ok, wsclosed};
        {'EXIT', Pid, shutdown} -> % ws closed
            boss_wamp:delete_agent(SessionId, Topic),
            lager:debug("ws_agent exit for reason {shutdown} "
                        "on topic ~p "
                        "for sessionId ~p, no action taken"
                       ,[Pid, Topic, SessionId]),
            {ok, shutdown};

        {'EXIT', Pid, Reason} ->
            boss_wamp:delete_agent(SessionId, Topic),
            lager:debug("ws_agent exit for reason ~p sup (~p) "
                        "ws_agent (~p) on topic ~p "
                        "for sessionId ~p, restart ws_agent"
                       ,[Reason, Pid, Topic, SessionId]),
            start_ws_agent_sup(Module, Topic, Since, FrameCtx);        
         _ ->
            loop_receive_ws_agent_sup(Agent, Module, Since, Topic, FrameCtx)
    end.

ws_agent(Module, Topic, Since, #frame_ctx{websocket_id=Addressee, 
                                                 session_id=SessionId}=FrameCtx) ->
    erlang:monitor(process, Addressee),
    Me = self(),
    tinymq:subscribe(Topic, Since, Me), %%todo: extend boss_mq:subscribe ?
    receive
        {_From, Timestamp, Messages} ->
            case call(Module, event, [Topic, Messages], FrameCtx) of
                {ok, {Topic1, Events}} -> 
                    maybe_notify(Topic1, Events, Addressee, SessionId);
                Err ->
                    lager:debug("Wamp EVENT error ~p", [Err]),
                    Err
            end,        
            ws_agent(Module, Topic, Timestamp, FrameCtx);
        
        {'DOWN', _Ref, process, Addressee, Reason} ->
            lager:debug(">>> ws agent ~p on topic ~p terminate"
                        " cuz consumer ~p terminate for reason ~p",
                       [Me, Topic, Addressee, Reason]),
            exit(wsclosed);

        info ->
            lager:info(">>> agent ~p subscribe to topic ~p forward to ~p",
                       [Me, Topic, Addressee]),
            ws_agent(Module, Topic, Since, FrameCtx);            

        unsubscribe ->
            exit(unsubscribe);         

        shutdown ->
            exit(shutdown);         

        _ ->
            ws_agent(Module, Topic, Since, FrameCtx)            
    end.

wamp_publish({ok, Args}, FrameCtx) ->  wamp_publish1(Args, FrameCtx);
wamp_publish(Err, _) ->  lager:debug("wamp publish error ~p", [Err]).

wamp_publish1(Args, #frame_ctx{session_id=SessionId}=FrameCtx) when is_list(Args)->
    Topic     = proplists:get_value(topic, Args),
    Event     = proplists:get_value(event, Args),
    ExcludeMe = proplists:get_value(exclude_me, Args, false),
    Exclude0  = proplists:get_value(exclude, Args, []),
    Eligible  = proplists:get_value(eligible, Args, []),    

    Exclude   = case ExcludeMe of                        
                    true ->
                        [SessionId | Exclude0];
                    _ ->
                        Exclude0
                end,
    WampEvent = #wamp_event{
                   event     =Event, 
                   exclude_me=ExcludeMe, 
                   exclude   =Exclude, 
                   eligible  =Eligible
                  },
    wamp_publish1(Topic, WampEvent, FrameCtx).
    
                
wamp_publish1(Topic, Event, FrameCtx) when is_binary(Topic)->
    wamp_publish1(binary_to_list(Topic), Event, FrameCtx);
wamp_publish1(Topic, Event, _FrameCtx) ->
    lager:debug("boss_mq:push(~p, ~p)", [Topic, Event]),
    boss_mq:push(Topic, Event).

maybe_notify(Topic, Events, Addressee, SessionId) when is_list(Events) ->
    [maybe_notify1(Topic, X, Addressee, SessionId) || X <- Events].

maybe_notify1(Topic, #wamp_event{event=Event,
                                exclude_me=false, 
                                exclude=[], 
                                eligible=[]}, Addressee, _) ->    
    notify(Topic, Event, Addressee);

maybe_notify1(Topic, #wamp_event{  event=Event, 
                                 exclude=Exclude, 
                                eligible=Eligible}, Addressee, SessionId) ->    
    case lists:member(SessionId, Eligible) of
        true ->
            notify(Topic, Event, Addressee);            
        false ->
            case lists:member(SessionId, Exclude) of 
                true ->
                    lager:debug("Wamp Event ~p exclude true ~p in ~p", 
                                [Event, SessionId, Exclude]);
                false ->
                    notify(Topic, Event, Addressee)
            end
    end.
                    
notify(Topic, Event, Addressee) ->
    Msg = [?WAMP_EVENT, Topic, Event],
    Json = ?json_encode(Msg),
    lager:debug("Wamp EVENT ~p notify ~p", [Msg, Addressee]),
    Addressee ! {text, Json}.


