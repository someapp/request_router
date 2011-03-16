-module(request_router_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start()->
    start(normal, []).

start(_StartType, _StartArgs) ->
    load_config(),
    ensure_started([inets, sasl, os_mon, gproc]),
    request_router_sup:start_link().

stop(_State) ->
    cleanup(),
    request_router_sup:terminate_child(),
    ok.


load_config()->
    app_util_config:read_config([], configuration_spec()).

configuration_spec()->
    [].
    
ensure_started([]) -> ok;
ensure_started(Apps) when is_list(Apps)->
    app_util:ensure_started(Apps).   

cleanup()->
    ok.



