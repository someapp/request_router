-module(request_router_config).

-behaviour(gen_server).

-include("../include/log.hrl").

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

-define(SERVER, ?MODULE).
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
-spec stop() -> ok | {error, term()}.
stop()->
    gen_server:call({stop, normal}).

%% @doc Get listening port number. Required configuration parameter.
%% @end    
-spec listening_port() -> pos_integer().
listening_port()->
    call(listening_port).
    
%% @doc Get Maximum requests waiting in queue for process. Required configuration parameter.
%% @end   
-spec max_queued_reqs() -> pos_integer().
max_queued_reqs()->
    call(max_queued_reqs).

%% @doc Get supported client protocol. Required configuration parameter.
%% @end   
-spec client_protocol() -> pos_integer().
client_protocol()->
    call(client_protocol).
    
%% @doc Get logger file name. Default is request_router_log.log
%% @end   
-spec log_name() -> file:name().    
log_name()->
    call(log_name).
 
%% @doc Get folder path of storing log files. Default is he current working directory
%% @end   
-spec log_dir() -> file:name().    
log_dir()->
    call(log_dir).

%% @doc Get connection timeout. Default is 5 sec.
%% @end 
-spec connection_timeout()-> pos_integer().
connection_timeout()->
    call(connection_timeout).

%% @doc Get request processing timeout. Default is 5 sec.
%% @end 
-spec req_process_timeout() -> pos_integer().
req_process_timeout() ->
    call(req_process_timeout).
    
%% @doc Get boolean flag whether to start with profiling. Default is false.
%% @end   
-spec should_profile() -> boolean().    
should_profile()->
    call(should_profile).
 
%%====================================================================

%% private functions

config_spec() ->
    [
      required_key(listening_port),
      required_key(max_queued_reqs),
      required_key(client_protocol),
      optional_key(log_dir, "./log"),
      optional_key(log_name, "request_router_log.log"),
      optional_key(connection_timeout, 5),
      optional_key(req_process_timeout, 5),
      optional_key(should_profile, false)
    ].

% @doc Initialize the configs into memory
% @end
init([])->
    {ok, #state{ conf = read_config([]) }}.

% @doc Create a list of {configKey, value}
% @end
read_config(Config) ->     
   [F(Config) || F <- config_spec()].    
    
call(Message) when is_atom(Message) ->
    gen_server:call(?MODULE, {get_param, Message}).

% @doc Getting optional key or use default
% @end    
optional_key(KeyName, Default) when is_atom(KeyName) ->
    app_config_util:optional(?APPNAME, KeyName, Default).
    
% @doc Getting required key
% @end
required_key(KeyName) when is_atom(KeyName) -> 
   app_config_util:required(?APPNAME, KeyName).    	

handle_call({get_param, listening_port}, _From, State)->
   handle_message(listening_port, State);    
handle_call({get_param, max_queued_request}, _From, State)->
   handle_message(max_queued_request, State);
handle_call({get_param, client_protocol}, _From, State)->
   handle_message(client_protocol, State);
handle_call({get_param, log_name}, _From, State)->
   handle_message(log_name, State);
handle_call({get_param, log_dir}, _From, State)->
   handle_message(log_dir, State);  
handle_call({get_param, connection_timeout}, _From, State)->
   handle_message(connection_timeout, State);    
handle_call({get_param, req_process_timeout}, _From,  State)->
   handle_message(req_process_timeout, State);
handle_call({get_param, Message}, _From, _State) ->
    error({unsupported_key, Message});
%% @doc Ignore all unspported Message
%% @end
handle_call(AllMsg, _From, State)->
    ?WARN(io:format("Unrecognised message: ~p",[AllMsg])),
    {reply, ok, State}.    


handle_message(Key, #state { conf = Conf } = State) ->
    Value = proplists:get_value(Key, Conf),
    {reply, Value, State}.
    
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
terminate(_Reason, _State)->
    ok.

%% @doc Perform Hot code swap and upgrade
%% @end
code_change(_OldVer,State, _Extra)->
    {ok, State}.



