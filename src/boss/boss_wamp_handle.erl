-module(boss_wamp_handle).

-export([process_frame/3]).

-include("boss_wamp.hrl").

-define(json_encode(X), jsx:encode(X)).

process_frame([?WAMP_PREFIX, _Prefix, _Uri], _FrameCtx, State) ->
    {ok, State};
process_frame([?WAMP_CALL, CallId, Uri | Args], FrameCtx, State) ->
    {Mod, Fun} = get_MF_from_uri(Uri),
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

process_frame([?WAMP_SUBSCRIBE, _Topic], _FrameCtx, State) ->
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


get_MF_from_uri(_Uri) ->
   {module, function}.    
