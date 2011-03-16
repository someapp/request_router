-module(request_router_config).

-behaviour(gen_server).

-export([start/0, stop/0,
         listening_port/0,
         max_queued_reqs/0,
         client_protocol/0,
         log_dir/0,
         log_name/0,
         profiling/0 
        ]).


%% gen_server callbacks
-export([init/1, 
         terminate/2, 
         code_change/3, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2]).
  
-record(state, { conf :: [{atom(), term()}]}).  
         
start()->
    gen_server:call
    
stop()->
    
    gen_server:call({stop, normal, _State}).
    
listening_port()->
    gen_server:call({}).
    
max_queued_reqs()->
    gen_server:call({}).

client_protocol()->
    gen_server:call({}).
    
log_name()->
    gen_server:call({}).
    
log_dir()->
    gen_server:call({}).
    
profiling()->
    gen_server:call({}).
    
%%====================================================================

%% private functions

init([])->
    

handle_call({})->

handle_call({})->

handle_call({})->

handle_call({})->

handle_call({})->

handle_call({})->
   

handle_call(AllMsg, State)->
    {reply, ok, State}

handle_cast(_Msg, State)->
    {no_reply, State}.
    
handle_info(_Info, State)->
    {no_reply, State}.

terminate(_Reason, State)->
    ok.

code_change(_OldVer,State, _Extra)->
    {ok, State}.


