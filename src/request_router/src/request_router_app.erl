-module(request_router_app).

-behaviour(application).

%% Request router going always to be start up with a custom config
%% A default config same name as module is assumed if no config supplied
-export([start/0, start/1]).

%% Application callbacks
-export([start/2, stop/1]).

-include("log.hrl").

-define (DEPENDANT_APPS, [inets, sasl, os_mon, gproc]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start()->
    start(normal, []).

% App config already in erlang list of terms. load it in environment
start(Config) when is_list(Config)->
        
    ensure_started(?DEPENDANT_APPS),
     
     .
 
%% ===================================================================
%% @doc All the required dependent services are started here. All optional
%% services are also listed. Services classified as local and global meaning
%% named-service with visibility in local VM versus globally visible.
%% 
%% Required services: 
%% request_router_listener (local) - inet traffic listener 
%% request_router_dht (global) - stores the pids and *querable meta data* of workers; 
%%                            data periodic update
%% request_resource_manager (local) - deals with how to choose optimal 
%%                           set of availble workers
%% request_router_acceptor (local)- take over ownership of request, handler it over to 
%%                           processing handler
%% request_router_logger (local)- as logging events sink 
%% request_router_eventmanager (local)- as plugin socket for new event handler
%% request_router_counter (local) - stores statistic counters for local service 
%% Optional service:
%% request_router_profiler (local) - for computing resource profiling
%% request_router_rest (global) - restful interface for operational and maintaince functions 
%% @end
%% ===================================================================
start(_StartType, _StartArgs) ->   
    ?INFO("Starting Request Router ... "), 
    ensure_started(?DEPENDANT_APPS),
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

required_serivce(ServiceName) when is_atom(ServiceName) ->
    ?CHILD().
    
optional_service(ServiceName) when is_atom(ServiceName)->
    case request_router_config

load_config()->
    app_util_config:read_config([], configuration_spec()).

    
ensure_started([]) -> ok;
ensure_started(Apps) when is_list(Apps)->
    app_util:ensure_started(Apps).   
    

% Put all the external resource clean up in right order here
cleanup()->
    ok.
    
% ================= Extra Parsed in Config values ======    
load_config([])->
    ok.    

load_config([Key|Rest) ->
    ok = application:set_env("").
    load_config(Rest).    
    
    
% ================= Profiling Section ==================
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

profile_output() ->
    eprof:stop_profiling(),
    eprof:log("procs.profile"),
    eprof:analyze(procs),
    eprof:log("total.profile"),
    eprof:analyze(total).    



