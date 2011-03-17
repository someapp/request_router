-module(request_router_app).

-behaviour(application).

%% Request router going always to be start up with a custom config
%% A default config same name as module is assumed if no config supplied
-export([start/0, start/1]).

%% Application callbacks
-export([start/2, stop/1]).
-include("log.hrl").

-define (Dependent_apps, [inets, sasl, os_mon, gproc]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start()->
    start(normal, []).

% App config already in erlang list of terms. load it in environment
start(Config) when is_list(Config)->
     load_config(Config),
     ensure_started(?Dependent_apps)
     .
 
%% ===================================================================
%% 
%%
%% ===================================================================
start(_StartType, _StartArgs) ->   
    ?INFO("Starting Request Router ... "), 
    ensure_started(?Dependent_apps),
    Ret = case request_router_sup:start_link() of
         {ok, Pid} -> 
              ok = 
         
         {error, Reason} -> {error, Reason}
    end.

stop(_State) ->
    ?INFO("Stopping Request Router"),
    cleanup(),
    request_router_sup:terminate_child(),
    ok.

load_config()->
    app_util_config:read_config([], configuration_spec()).

consider_profiling() ->
    case should_profile() of
	    true ->
            	eprof:start(),
            	eprof:start_profiling([self()]);
	    false ->
	        ignore
    end.

should_profile()-> 
    request_router_config:should_profile().

configuration_spec()->
    [].
    
ensure_started([]) -> ok;
ensure_started(Apps) when is_list(Apps)->
    app_util:ensure_started(Apps).   

% Put all the external resource clean up in right order here
cleanup()->
    ok.



