-module(boss_static_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([find_file/2]).
-export([do_gzip_on_disc/3]).

-record(state, {
        static_tables
    }).

-record(static_asset, {
        path,
        size,
        count,
        gzip,
        gzip_size,
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
    Name = list_to_atom(atom_to_list(App)++"_static_asset"),
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
    BaseDir = boss_files:root_priv_dir(App),
    StaticDir = filename:join(BaseDir, StaticPrefix),    
    StaticGzipDir = filename:join(BaseDir, StaticPrefix ++ "_gzip"),
    filelib:ensure_dir(StaticGzipDir),
    Size=length(StaticDir)+1, 
    DoGzipIt = fun(File) ->
                       FileGzip=filename:join(StaticGzipDir,lists:nthtail(Size, File)), 
                       %%fit into memory :)
                       {ok, Data} = file:read_file(File),
                       Len0 = byte_size(Data),
                       Bin = zlib:gzip(Data),
                       Len1 = byte_size(Bin),
                       Ratio = (Len0-Len1)/Len0*100,
                       case Ratio of
                           %maybe configurable Ratio sup to 2% 
                           Ratio when Ratio > 2 ->
                               filelib:ensure_dir(FileGzip),
                               file:write_file(FileGzip, Bin);                   
                           _ ->
                               %don't store it
                               ok
                       end
               end,
    find_file(StaticDir, DoGzipIt).
            
find_file(Path, FunFile) when is_function(FunFile)->    
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
            case file:list_dir(Path) of
                {erorr, _}=Err ->                    
                    Err;
                {ok, List} ->
                    lists:foreach(Fun, List)
            end
    end.

do_load_gzip_on_cache(App, File) ->
    ok.

                        

            
