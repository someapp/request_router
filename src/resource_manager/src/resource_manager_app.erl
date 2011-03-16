-module(resource_manager_app).

-behaviour(application).

%%
-export().

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    resource_manager_sup:start_link().

stop(_State) ->
    ok.
