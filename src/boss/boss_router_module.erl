%% Author: mihawk
%% Created: 05/12/2014

%% Description: Minimalist module based Router system for Chicago Boss 
%% ===================================================================
%%  to play with it:
%%  in each app lib folder write a <app_name>_routes.erl module,
%%  it is a first draft, it is based on the current one
%%
%% -module(<app_name>_routes).
%%
%% -export([
%%           domains/0
%%          ,route/1
%%          ,unroute/3
%%          ,handle/1
%%         ]).
%%
%% domains() -> all. %% ["mydomain.com","toto.com"]
%%
%% %% your routes
%% route("/") ->  {"<app>_<name>_controller", "index", []};
%% route(  _) ->  undefined.            
%%
%% %% used for redirect & moved directive --> Location" : "Url"
%% unroute(Controller, Acction, Params) -> Url;
%% unroute(         _,       _,      _) -> undefined.
%%  
%% %%handle(404) -> {Controller, Action, Params};
%% handle(_) -> undefined.
%%
%%

-module(boss_router_module).
-behaviour(boss_router_adapter).

%%
%% Exported Functions
%%
-export([start/0, 
         start/1, 
         stop/0]).
-export([find_application_for_path/3]).
-export([
         reload/1
         ,route/2
         ,unroute/6
         ,handle/2 
         ,get_all/1
         ,set_controllers/2
         ,router_pid/1
        ]).

%%
%% API Functions
%%

start() ->
    start([]).


start(Options) ->
    lager:info("starting router module based routing (experimental)..."),
    App = proplists:get_value(application, Options),
    {ok, App}.

stop() ->
    lager:info("stoping router module based routing (experimental)..."),
    ok.

reload(App) ->	
    %% TODO: recompile ?? boss_router_compiler ??
    ok.

router_pid(App) ->
    App.

route(App, Url) ->
    RouteModule = list_to_atom(atom_to_list(App) ++ "_routes"),
    Controllers = boss_files:web_controller_list(App), 
    case RouteModule:route(Url) of
        undefined ->
            case string:tokens(Url, "/") of 
                [Controller] -> 
                    case is_controller(App, Controller, Controllers) of
                        true -> {ok, {App, Controller, default_action(App, Controller, Controllers), []}};
                        false -> not_found
                    end;
                [Controller, Action|Tokens] ->
                    case is_controller(App, Controller, Controllers) of
                        true -> 
                            UnquotedTokens = lists:map(fun mochiweb_util:unquote/1, Tokens),
                            {ok, {App, Controller, Action, UnquotedTokens}};
                        false -> not_found
                    end;
                _ ->
                    not_found
            end;
        {C, A, P} -> 
            case boss_files:web_controller(App, C, Controllers) of
                undefined ->
                    not_found;
                ModuleName ->
                    ControllerModule = list_to_atom(ModuleName),
                    {Tokens, []}     = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
                    {ok, {App, C, A, Tokens}}
            end
    end.


unroute(Pid, Application, ControllerList, Controller, undefined, Params) ->
    case boss_files:web_controller(Application, Controller, ControllerList) of
        undefined -> not_found;
        ModuleName ->
            ControllerModule =  list_to_atom(ModuleName),
            Action =  case proplists:get_value(default_action, ControllerModule:module_info(attributes)) of
                          [DefaultAction] when is_atom(DefaultAction) ->
                      atom_to_list(DefaultAction);
                          _ ->
                              "index"
                      end,
            unroute(Pid, Application, ControllerList, Controller, Action, Params)
    end;

unroute(Pid, Application, ControllerList, Controller, Action, Params) ->
    RouteModule = list_to_atom(atom_to_list(Application) ++ "_routes"),
    case RouteModule:unroute(Controller, Action, Params) of
        undefined ->
            case boss_files:web_controller(Application, Controller, ControllerList) of
                undefined -> not_found;
                ModuleName ->
                    ControllerModule = list_to_atom(ModuleName),
                    {Tokens, Variables1} = boss_controller_lib:convert_params_to_tokens(Params, 
                                                                                        ControllerModule, 
                                                                                        list_to_atom(Action)),                    
                    URL = case Tokens of
                              [] ->
                                  lists:concat(["/", Controller, "/", Action]);
                              _ ->
                                  lists:concat(["/", Controller, "/", Action |
                                                lists:foldr(fun(T, Acc) -> ["/", mochiweb_util:quote_plus(T) | Acc] end, [], Tokens)])
                          end,
                    QueryString = mochiweb_util:urlencode(Variables1),
                    case QueryString of
                        "" ->
                            URL;
                        _ ->
                            URL ++ "?" ++ QueryString
                    end;
                RoutedURL ->
                    RoutedURL
            end
    end.

handle(App, StatusCode) ->
    RouteModule = list_to_atom(atom_to_list(App) ++ "_routes"),
    case RouteModule:handle(StatusCode) of
        undefined -> not_found;
        Other -> Other
    end.
             
get_all(App) ->    
    RouteModule = list_to_atom(atom_to_list(App) ++ "_routes"),
    RouteModule:get_all().

set_controllers(_App, _Controllers) -> ok.

find_application_for_path(Req, Path, Applications) ->
    Host    = Req:header(host),
    UseHost = case Host of
        undefined -> undefined;
        _ -> hd(re:split(Host, ":", [{return, list}]))
    end,
    find_application_for_path(UseHost, Path, undefined, Applications, -1).

find_application_for_path(_Host, _Path, Default, [], _MatchScore) ->
    Default;
find_application_for_path(Host, Path, Default, [App|Rest], MatchScore) ->
    RouteModule = list_to_atom(atom_to_list(App) ++ "_routes"),
    DomainScore = case Host of
        undefined -> 0;
        _ ->
            case RouteModule:domains() of
                all     -> 0;
                Domains ->
                    case lists:member(Host, Domains) of
                        true  -> 1;
                        false -> -1
                    end
            end
    end,

    BaseURL   = RouteModule:base_url(),
    PathScore = length(BaseURL),
    
    {UseApp, UseScore} = 
        case (DomainScore >= 0) andalso (1000 * DomainScore + PathScore > MatchScore) andalso lists:prefix(BaseURL, Path)  of
        true  -> {App, DomainScore * 1000 + PathScore};
        false -> {Default, MatchScore}
    end,

    find_application_for_path(Host, Path, UseApp, Rest, UseScore).

is_controller(App, Controller, Controllers) -> 
    boss_files:is_controller_present(App, Controller, Controllers).

default_action(App, Controller, Controllers) ->
    case is_controller(App, Controller, Controllers) of
        true ->
            ControllerModule = 
                list_to_atom(boss_files:web_controller(App, Controller, Controllers)),
            case proplists:get_value(default_action, ControllerModule:module_info(attributes)) of
                [DefaultAction] when is_atom(DefaultAction) ->
                    atom_to_list(DefaultAction);
                _ ->
                    "index"
            end;
        false ->
            "index"
    end.
