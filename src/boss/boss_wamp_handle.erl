-module(boss_wamp_handle).

-export([process_frame/3]).

-include("boss_wamp.hrl").

-define(json_encode(X), jsx:encode(X)).

process_frame([?WAMP_PREFIX, _Prefix, _Uri], _ReqCtx, State) ->
    {ok, State};
process_frame([?WAMP_CALL, CallId, Uri | Args], ReqCtx, State) ->
    {Mod, Fun} = get_MF_from_uri(Uri),
    Reply = call(Mod, Fun, Args, ReqCtx),    
    Addresse = ReqCtx#req_ctx.websocket_id,
    ReplyJson = case Reply of 
                     {ok, Result} ->
                         ?json_encode([?WAMP_CALLRESULT, CallId, Result]);
                     {error, Reason} ->
                         ?json_encode([?WAMP_CALLERROR, CallId, Reason])                
                 end,    
    Addresse ! {text, ReplyJson},
    {ok, State};

process_frame([?WAMP_SUBSCRIBE, _Topic], _ReqCtx, State) ->
    {ok, State};
process_frame([?WAMP_UNSUBSCRIBE, _Topic], _ReqCtx, State) ->
    {ok, State};

process_frame([?WAMP_PUBLISH, _Topic, _Evt], _ReqCtx, State) ->
    {ok, State};
process_frame([?WAMP_PUBLISH, _Topic, _Evt, _ExcludeMe], _ReqCtx, State) ->
    {ok, State};
process_frame([?WAMP_PUBLISH, _Topic, _Evt, _Exclude, _Eligible], _ReqCtx, State) ->
    {ok, State}.

call(_Mod, undefined, _Args, _ReqCtx) ->   {error, <<"undefined function">>};
call(undefined, _Fun, _Args, _ReqCtx) ->   {error, <<"undefined module">>};
call(Module, Function, Args, ReqCtx) when is_binary(Module)->
    call(list_to_atom(binary_to_list(Module)), Function, Args, ReqCtx);
call(Module, Function, Args, ReqCtx) when is_binary(Function)->
    call(Module, list_to_atom(binary_to_list(Function)), Args, ReqCtx);
call(Module, Function, Args, ReqCtx) when is_atom(Module),
                                       is_atom(Function)->
    apply_function(Module, Function, Args, ReqCtx).


apply_function(Module, Function, Args, ReqCtx) when is_atom(Module) ->
    maybe_pmod(Module, Function, Args, ReqCtx).

maybe_pmod(Mod, Fun, Args, ReqCtx) ->
    case proplists:get_value(new, Mod:module_info(exports), undefined) of
        undefined ->
            erlang:apply(Mod, Fun, Args);
        N ->
            Req = proplists:get_value(request, ReqCtx),
            SessionID = proplists:get_value(session_id, ReqCtx),
            Instance = case N of
                           1 -> Mod:new(Req);
                           2 -> Mod:new(Req, SessionID);
                           3 -> Mod:new(Req, SessionID, ReqCtx)
                       end,
            erlang:apply(Instance, Fun, Args)
    end.


get_MF_from_uri(_Uri) ->
   {module, function}.    
