-module(boss_static_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        static_tables
    }).

-record(static_asset, {
        path,
        size,
        count,
        gzip,
        backend
    }).

start_link() ->
    start_link([]).

%% start_link(Config) ->
%%     gen_server:start_link({local, boss_static}, ?MODULE, Config, []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


init(_Options) ->
    {ok, #state{static_tables=[]}}.

handle_call({is_gzip, _App, _Url}, _From, State) ->
    {reply, ok, State}.

handle_cast({gzip_static_asset, App, StaticPrefix}, #state{static_tables=Tables}=State) when is_atom(App) ->
    Name = list_to_atom(atom_to_list(App)++"static_asset"),
    case proplists:get_value(App, Tables) of
        {ok, EtsTab} ->
            do_gzip_on_disc(EtsTab, App, StaticPrefix),
            {reply, ok, State};

        undefined ->
            NewTab = ets:new(Name, []),
            NewTables = [{App, NewTab}|Tables],
            do_gzip_on_disc(NewTab, App, StaticPrefix),            
            {reply, ok, State#state{static_tables=NewTables}}
    end;

handle_cast({gzip_static_asset, App, _StaticPrefix, _Url, on_disk}, #state{static_tables=Tables}=State) when is_atom(App) ->
    Name = list_to_atom(atom_to_list(App)++"static_asset"),
    NewTables = [{App, ets:new(Name)}|Tables],
    {reply, ok, State#state{static_tables=NewTables}};

handle_cast({gzip_static_asset, App, _StaticPrefix, _Url, on_cache}, #state{static_tables=Tables}=State) when is_atom(App) ->
    Name = list_to_atom(atom_to_list(App)++"static_asset"),
    NewTables = [{App, ets:new(Name)}|Tables],
    {reply, ok, State#state{static_tables=NewTables}};

handle_cast(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{static_tables=Tables}) ->
    [ets:delete(X) || X<- Tables].

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% internal 
do_gzip_on_disc(_EtsTab, _App,  _StaticPrefix) ->
    ok.
