%%%-------------------------------------------------------------------
%%% @author mihawk <mihawk@abc>
%%% @copyright (C) 2014, mihawk
%%% @doc
%%% @end
%%% Created : 16 Jan 2014 by mihawk <mihawk@abc>
%%%-------------------------------------------------------------------

-module(boss_wamp_handle).

-export([process_frame/3]).

-export([start_agent/4]).
-export([websocket_agent/4]).

-include("boss_wamp.hrl").

-define(json_encode(X), jsx:encode(X)).

process_frame([?WAMP_PREFIX, _Prefix, _Uri], _FrameCtx, State) ->
    {ok, State};
process_frame([?WAMP_CALL, CallId, Uri | Args], FrameCtx, State) ->
    Dir = State#state.directory,
    {Mod, Fun} = get_MF_from_uri(Uri, Dir),
    Reply = call(Mod, Fun, Args, FrameCtx),    
    Addresse = FrameCtx#frame_ctx.websocket_id,
    ReplyJson = case Reply of 
                     {ok, Result} ->
                         ?json_encode([?WAMP_CALLRESULT, CallId, Result]);
                     {error, Reason} ->
                         ?json_encode([?WAMP_CALLERROR, CallId, Reason])                
                 end,    
    Addresse ! {text, ReplyJson},
    {ok, State};

process_frame([?WAMP_SUBSCRIBE, TopicUri], FrameCtx, State) ->
    Dir = State#state.directory,
    {Mod, _Fun} = get_MF_from_uri(TopicUri, Dir),
    case call(Mod, suscribe, TopicUri, FrameCtx) of
        {ok, {Topic, Since}} ->
            % ?? process_flag(trap_exit, true),
            spawn(?MODULE, start_agent, [Mod, Topic, Since, FrameCtx]);
        Err ->
            lager:info("boss_mq:pull Error ~p", [Err]),            
            Err
    end,
    {ok, State};

process_frame([?WAMP_UNSUBSCRIBE, _Topic], _FrameCtx, State) ->
    {ok, State};

process_frame([?WAMP_PUBLISH, _Topic, _Evt], _FrameCtx, State) ->
    {ok, State};
process_frame([?WAMP_PUBLISH, _Topic, _Evt, _ExcludeMe], _FrameCtx, State) ->
    {ok, State};
process_frame([?WAMP_PUBLISH, _Topic, _Evt, _Exclude, _Eligible], _FrameCtx, State) ->
    {ok, State}.

call(_Mod, undefined, _Args, _FrameCtx) ->   {error, <<"undefined function">>};
call(undefined, _Fun, _Args, _FrameCtx) ->   {error, <<"undefined module">>};
call(Module, Function, Args, FrameCtx) when is_binary(Module)->
    call(list_to_atom(binary_to_list(Module)), Function, Args, FrameCtx);
call(Module, Function, Args, FrameCtx) when is_binary(Function)->
    call(Module, list_to_atom(binary_to_list(Function)), Args, FrameCtx);
call(Module, Function, Args, FrameCtx) when is_atom(Module),
                                       is_atom(Function)->
    apply_function(Module, Function, Args, FrameCtx).


apply_function(Module, Function, Args, FrameCtx) when is_atom(Module) ->
    maybe_pmod(Module, Function, Args, FrameCtx).

maybe_pmod(Mod, Fun, Args, FrameCtx) ->
    case proplists:get_value(new, Mod:module_info(exports), undefined) of
        undefined ->
            erlang:apply(Mod, Fun, Args);
        N ->
            Req = proplists:get_value(request, FrameCtx),
            SessionID = proplists:get_value(session_id, FrameCtx),
            Instance = case N of
                           1 -> Mod:new(Req);
                           2 -> Mod:new(Req, SessionID);
                           3 -> Mod:new(Req, SessionID, FrameCtx)
                       end,
            erlang:apply(Instance, Fun, Args)
    end.


get_MF_from_uri(Url, _Dir) ->
    CleanUrl = clean_url(Url),
    Prefix = proplists:get_value(prefix, CleanUrl),
    case Prefix of 
        undefined ->
            Uri = proplists:get_value(uri, CleanUrl),
            Module = boss_wamp:lookup_uri(Uri,2),
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


%% boss_mq or tinymq use only string as topic/channel
start_agent(Module, Topic, Since, ReqCtx) when is_binary(Topic) ->
    start_agent(Module, binary_to_list(Topic), Since, ReqCtx);

start_agent(Module, Topic, Since, FrameCtx) ->
    Agent = spawn_link(?MODULE, websocket_agent, [Module, Topic, Since, FrameCtx]),
    lager:info("start ws agent ~p on topic ~p~n", [Agent, Topic]),    
    receive 
        {'EXIT', _Pid, normal} -> %
            ok;
        {'EXIT', _Pid, shutdown} -> % manual termination, not a c
            ok;
        {'EXIT', _Pid, Reason} ->
            lager:notice("restart ws agent on topic ~p for reason ~p",
                         [Topic, Reason]),                
            start_agent(Module, Topic, Since, FrameCtx)
    end.

websocket_agent(Module, Topic, Since, #frame_ctx{websocket_id=Addressee}=FrameCtx) ->
    erlang:monitor(process, Addressee),
    Me = self(),
    tinymq:subscribe(Topic, Since, Me), 
    receive
        {_From, Timestamp, Messages} ->
            case call(Module, event, [Topic, Messages], FrameCtx) of
                {ok, {Topic, Event}} -> 
                    Msg = [?WAMP_EVENT, Topic, Event],
                    Json = ?json_encode(Msg),
                    Addressee ! {text, Json};
                Err ->
                    Err
            end,        
            websocket_agent(Module, Topic, Timestamp, FrameCtx);
        
        {'DOWN', _Ref, process, Addressee, Reason} ->
            lager:info(">>> agent ~p terminate cuz consumer ~p die for reason ~p",
                       [Me, Addressee, Reason]),
            stop;

        info ->
            lager:info(">>> agent ~p subscribe to topic ~p forward to ~p",
                       [Me, Topic, Addressee]),
            websocket_agent(Module, Topic, Since, FrameCtx);            

        shutdown ->
            lager:info(">>> agent ~p terminate (shutdown)", [Me]),
            exit(shutdown);            

        _ ->
            websocket_agent(Module, Topic, Since, FrameCtx)            
    end.
