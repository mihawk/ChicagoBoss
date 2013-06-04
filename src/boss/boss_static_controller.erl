-module(boss_static_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([find_file/2]).

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


-define(is_folder(X),filelib:is_dir(X)).
-define(is_file(X),filelib:is_file(X)).


start_link() ->
    start_link([]).



%% start_link(Config) ->
%%     gen_server:start_link({local, boss_static}, ?MODULE, Config, []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    Apps = proplists:get_value(applications, Options),
    %priv/var/static/ets
    {ok, #state{static_tables=[]}}.

handle_call({set_on_cache, _App, _Url}, _From, State) ->
    {reply, ok, State};

handle_call({set_gzip_on_cache, _App, _Url}, _From, State) ->
    {reply, ok, State};

handle_call({is_gzip, _App, _Url}, _From, State) ->
    {reply, ok, State}.

handle_cast({gzip_static_asset, App, StaticPrefix}, #state{static_tables=Tables}=State) 
  when is_atom(App) ->
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
    Name = list_to_atom(atom_to_list(App)++"_static_asset"),
    NewTables = [{App, ets:new(Name)}|Tables],
    {reply, ok, State#state{static_tables=NewTables}};

handle_cast({incr_count, _App, _Url}, State) ->
    {noreply, State};

handle_cast(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{static_tables=Tables}) ->
    [ets:delete(X) || X<- Tables].

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% internal 
do_gzip_on_disc(_EtsTab, App,  StaticPrefix)
    when is_atom(App) ->
    GzipStatic = StaticPrefix ++ "_gzip",
    filelib:ensure_dir(GzipStatic),
    DoGzipIt = fun(X) ->
                      X
              end,
    Path = boss_file:static(App),
    find_file(Path, DoGzipIt).
    
        
find_file(Path, FunFile) ->    
    case ?is_folder(Path) of
        false ->
            ok;
        true ->
            Fun = fun(X) ->
                          Path1 = filename:absname_join(Path, X),
                          case ?is_folder(Path1) of 
                              true ->
                                  spawn(fun() ->
                                                find_file(Path1, FunFile)
                                        end);
                              false ->
                                  case ?is_file(Path1) of
                                      true ->
                                          FunFile(Path1);
                                      false ->
                                          ok
                                  end
                          end
                  end,
            case filelib:wildcard(Path) of
                {erorr, _}=Err ->                    
                    Err;
                {ok, List} ->
                    lists:foreach(Fun, List)
            end
    end.

                        

            
