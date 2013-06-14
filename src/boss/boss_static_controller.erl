-module(boss_static_controller).

-behaviour(gen_server).

-export([start_link/0]).
-export([start_link/1]).
%% gen_server API
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% boss_static API
-export([find_file/2]).
-export([do_gzip_on_disc/3]).

-record(state, {static_tables}).

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

start_link(Args) ->
    gen_server:start_link({local, boss_static}, ?MODULE, Args, []).

init(_Options) ->
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
            {noreply, State};
        
        undefined ->
            StaticTableId = ets:new(Name, [ordered_set, public, {keypos, 1}]),
            NewTables = [{App, StaticTableId}|Tables],
            do_gzip_on_disc(StaticTableId, App, StaticPrefix),            
            {noreply, State#state{static_tables=NewTables}}
    end;

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
do_gzip_on_disc(EtsTab, App,  StaticPrefix)  ->
    do_gzip_on_disc(EtsTab, App,  StaticPrefix, 2). %default 2%

do_gzip_on_disc(EtsTabId, App,  StaticPrefix, Limit) 
    when is_atom(App) ->
    BaseDir = boss_files:root_priv_dir(App),
    StaticDir = filename:join(BaseDir, StaticPrefix),    
    StaticGzipDir = filename:join(BaseDir, StaticPrefix ++ "_gzip"),
    filelib:ensure_dir(StaticGzipDir),
    Size=length(StaticDir)+1, 
    DoGzipIt = fun(File) ->
                       Path = lists:nthtail(Size, File),
                       FileGzip=filename:join(StaticGzipDir, Path), 
                       {ok, Data} = file:read_file(File),
                       Len0 = byte_size(Data),
                       Bin = zlib:gzip(Data),
                       Len1 = byte_size(Bin),
                       Ratio = case Len0 of 
                                   Len0 when Len0 /= 0 -> 
                                       (Len0-Len1)/Len0*100;
                                   _ -> undefined
                               end,
                       case Ratio of
                           Ratio when Ratio > Limit ->
                               filelib:ensure_dir(FileGzip),
                               file:write_file(FileGzip, Bin),
                               error_logger:info_msg("~p ~p~n",[Ratio, FileGzip]),
                               SA = #static_asset{
                                       path=list_to_binary(Path),
                                       size=Len0,
                                       count=0,
                                       gzip=true,
                                       gzip_size=Len1,
                                       backend=on_disc},
                               ets:insert(EtsTabId, SA);
                           _ ->                               
                               SA = #static_asset{
                                       path=list_to_binary(Path),
                                       size=Len0,
                                       count=0,
                                       gzip=false,
                                       gzip_size=0,
                                       backend=on_disc},
                               ets:insert(EtsTabId, SA)
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


                        

            
