-module(boss_static).

-export([
        gzip/1,
        is_gzip/2
	]).

gzip(App) ->
    gen_server:cast(boss_static, {gzip_static_asset, self(), App}).

is_gzip(App, Url) ->
    gen_server:call(boss_static, {is_gzip, App, Url}).

