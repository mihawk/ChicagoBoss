-module(boss_static).

-export([gzip/2]).
-export([is_gzip/2]).

-export([stop/0]).
-export([start/0]).
-export([start/1]).


start() ->
    start([]).

start(Options) ->
    boss_static_sup:start_link(Options).

stop() ->
    ok.

gzip(App, StaticPrefix) ->
    gen_server:cast(boss_static, {gzip_static_asset, App, StaticPrefix}).

is_gzip(App, Url) ->
    gen_server:call(boss_static, {is_gzip, App, Url}).

