%%%-------------------------------------------------------------------
%%% @author mihawk <mihawk@abc>
%%% @copyright (C) 2014, mihawk
%%% @doc
%%% @end
%%% Created : 16 Jan 2014 by mihawk <mihawk@abc>
%%%-------------------------------------------------------------------
-module(boss_wamp).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/0]).

-export([welcome/4]). 
-export([incoming/6]). 
-export([close/6]). 


%% gen_server callbacks

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([init_wamp_directory/0]).

-export([insert_prefix/3, 
         lookup_prefix/1, lookup_prefix/2, 
         delete_prefix/1]).
-export([insert_agent/3, lookup_agent/2, delete_agent/2]).

-include("boss_wamp.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

%% @doc Start the boss_wamp gen_server.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

-spec start_link(atom(), binary()) -> {ok, pid()}.
start_link(Handler, ServiceUrl) when is_atom(Handler)->
    gen_server:start_link({global, Handler}, ?MODULE, [Handler, ServiceUrl], []).

welcome(ServiceUrl, WebSocketId, Req, SessionId) ->
    gen_server:call({global, ?MODULE}, {welcome, ServiceUrl, WebSocketId, Req, SessionId}).
close(Reason, ServiceUrl, WebSocketId, Req, SessionId, WampId) ->
    gen_server:cast({global, ?MODULE}, {terminate, Reason, ServiceUrl, WebSocketId, Req, SessionId, WampId}).
incoming(ServiceUrl, WebSocketId, Req, SessionId, Message, WampId) ->
    gen_server:cast({global, ?MODULE}, {frame, ServiceUrl, WebSocketId, Req, SessionId, Message, WampId}).


insert_prefix(Prefix, Module, WebSocketId) ->
    ets:insert_new(?TAB, {{wamp_prefix, Prefix}, Module, WebSocketId}). 
lookup_prefix(Prefix) ->
    ets:lookup(?TAB, {wamp_prefix, Prefix}).
lookup_prefix(Prefix, Pos) ->
    ets:lookup_element(?TAB, {wamp_prefix, Prefix}, Pos).
delete_prefix(Prefix) ->
    ets:delete(?TAB, {wamp_prefix, Prefix}).

insert_agent(Agent, SessionId, Topic) when is_pid(Agent)->
    ets:insert_new(?TAB, {{wamp_agent, SessionId, Topic}, Agent}). 
lookup_agent(SessionId, Topic) ->
    ets:lookup_element(?TAB, {wamp_agent, SessionId, Topic}, 2).
delete_agent(SessionId, Topic) ->
    ets:delete(?TAB, {wamp_agent, SessionId, Topic}).

    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_) ->
    process_flag(trap_exit, true),
    lager:info("Starting Wamp service on ~p~n", [node()]),
    {ok, #state{wamp_directory=init_wamp_directory()}}.
    
handle_call({welcome, _ServiceUrl, WebSocketId, Req, _SessionId}, _From, State) ->    
    ServerInfo = "ChicagoBoss WAMP Server/" ++ ?__VERSION__,
    WampId = generate_id(), %% WampId = SessionId    
    WelcomeMsg = [?WAMP_WELCOME, list_to_binary(WampId), ?WAMP_PROTOCOL_VERSION,
                  list_to_binary(ServerInfo)],    
    Req1 = cowboy_req:set_resp_header(<<"Sec-Websocket-Protocol">>, <<"wamp">>, Req),
    BinMsg = ?json_encode(WelcomeMsg),
    lager:debug("WAMP welcome frame ~p for SessionId ~p", [BinMsg, WampId]),
    WebSocketId ! {text, BinMsg},
    %%todo call init  <app>_wamp:init() ??
    {reply, {WampId, Req1}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({terminate, _Reason, _ServiceUrl, _WebSocketId, _Req, _SessionId, _WampId}, State) ->
    %%todo call terminate  <app>_wamp:terminate() ??
    {noreply, State};

handle_cast({frame, ServiceUrl, WebSocketId, Req, SessionId, Frame, WampId}, State) ->
    handle_frame(ServiceUrl, WebSocketId, Req, SessionId, Frame, WampId, State),
    {noreply, State};

handle_cast(_Msg, State) ->    
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    %error_logger:info_msg("** handle_info ~p~n",[_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    error_logger:info_msg("** terminate ~p~n",[_Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_frame(ServiceUrl, WebsocketId, Req, SessionId, JsonFrame, WampId, State) ->
    Message = ?json_decode(JsonFrame),
    lager:debug("receive frame: ~p", [Message]),
    FrameCtx = #frame_ctx{service_url = ServiceUrl, 
                      request = Req,
                      session_id = SessionId,
                      wamp_id = WampId,
                      websocket_id = WebsocketId},
    %%spawn a process for each frame
    process_flag(trap_exit, true),    
    _Pid = spawn_link(fun() ->
                             boss_wamp_handle:process_frame(Message, FrameCtx, State)
                      end),    
    {ok, State}.

init_wamp_directory() ->    
    Applications = boss_env:get_env(applications, []),
    AllModuleList = list_wamp_modules(Applications),
    WampProcHandler = boss_env:get_env(wamp_procedure_handler, wamp_procedure),    
    Mapping = boss_files:wamp_mapping(AllModuleList) ++ 
        [{<<"http://api.wamp.ws/procedure">>, WampProcHandler}],
    lager:debug("Wamp Mapping ~p", [Mapping]), 
    fill_dict(Mapping ++ [], dict:new()).

list_wamp_modules(L) ->
    list_wamp_modules(L, []).
list_wamp_modules([], Acc) ->
    Acc;
list_wamp_modules([H|T], Acc) ->
    list_wamp_modules(T, boss_files:wamp_list(H) ++ Acc).
    
fill_dict([], Dict) -> Dict;
fill_dict([{K,V}|L], Dict) -> 
    fill_dict(L, dict:store(K,V, Dict)).

generate_id() ->
    get_random_string(16, ?ALLOWEDCHARS).

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).
