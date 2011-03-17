-module(request_router_config).

-behaviour(gen_server).

-include("log.hrl").

-export([start/0, stop/0,
         listening_port/0,
         max_queued_reqs/0,
         client_protocol/0,
         log_dir/0,
         log_name/0,
         connection_timeout/0,
         req_process_timeout/0,
         should_profile/0 
        ]).


%% gen_server callbacks
-export([init/1, 
         terminate/2, 
         code_change/3, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2]).

-define(SERVER, MODULE).
-define(APPNAME, request_router).
  
-record(state, { conf :: [{atom(), term()}]}).  

%% ====================================================================

%% @doc Start the request router config `gen_server' governor.
%% @end       
-spec start() -> {ok, pid()} | ignore | {error, term()}.   
start()->
    Args = [],
    Options = [],
    gen_server:start_link(?SERVER, ?MODULE, Args, Options).
    
%% @doc Stop normally the request router config server 
%% @end   
stop()->
    gen_server:call({stop, normal, _State}).
    
listening_port()->
    call(listening_port).
    
max_queued_reqs()->
    call(max_queued_reqs).

client_protocol()->
    call(client_protocol).
    
log_name()->
    call(log_name).
    
log_dir()->
    call(log_dir).
    
should_profile()->
    call(should_profile).
.    
%%====================================================================

%% private functions

init([])->
    
    .
    
call(Message) when is_atom(Message) ->
    gen_server:call(?MODULE, {get_param, Message}).
    
optional_key(KeyName, Default) when is_atom(KeyName) ->
    app_config_util:optional(?APPNAME, KeyName, Default).

required_key(KeyName) when is_atom(KeyName) -> 
    app_config_util:required(?APPNAME, KeyName).    	

handle_call({get_param, listening_port}, From, State)->
    
handle_call({get_param, max_queued_request}, From, State)->

handle_call({get_param, client_protocol}, From, State)->

handle_call({get_param, log_name}, From, State)->

handle_call({get_param, log_dir}, From, State)->

handle_call({get_param, should_profile}, From State)->
   

handle_call({get_param, Message}, _From, _State) ->
    error({unsupported_key, Message}).

%% @doc Ignore all unspported Message
%% @end
handle_call(AllMsg, State)->
    {reply, ok, State}.
    
%% @doc Ignore all unspported Message
%% @end
handle_cast(_Msg, State)->
    {no_reply, State}.

%% @doc Ignore all unspported Message
%% @end    
handle_info(_Info, State)->
    {no_reply, State}.

%% @doc Terminate request router config server
%% @end
terminate(_Reason, State)->
    ok.

%% @doc Perform Hot code swap and upgrade
%% @end
code_change(_OldVer,State, _Extra)->
    {ok, State}.



