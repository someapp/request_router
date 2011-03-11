-module (app_util).

-author (edwardt.tril@gmail.com).

-export([ensure_app_start/1, 
         ensure_app_stop/1,
         get_all_value/1,
         get_value/2,
         get_value/3,
         set_value/3,
         required/2,
	 optional/3,
	 read_config/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


ensure_app_start(AppName) when is_atom(AppName) ->
  case application:start(AppName) of 
       ok -> ok;
       {error, {already_started, _}} -> ok;
       {error, ERROR} -> {error, ERROR}
  end.

ensure_started([]) -> ok;
ensure_started([App|Rest]) when is_atom(App)->
  ensure_app_start(App),
  ensure_started([Rest]).

ensure_app_stop(AppName) when is_atom(AppName) ->
  case application:stop(AppName) of
       ok -> ok;
       {error, {"no such file or directory", App}} ->
               {error, {"no such file or directory", App}};
       {error, {not_started, _}} -> ok;
       {error, ERROR} -> {error, ERROR}
  end.  

ensure_stopped([])-> ok;
ensure_stopped([App|Rest]) when is_atom(App) ->
  ensure_app_stop(App),
  ensure_stopped([Rest]).

get_all_value(App) when is_atom(App)->
  application:get_all_env(App).

get_value(App, Key) when is_atom(Key) ->
  case application:get_env(App, Key) of 
      {ok, Value} ->
           {ok, Value};
      _Else ->
           undefined
  end.  

get_value(App, Key, Default) when is_atom(Key) ->
  case application:get_env(App, Key) of 
      {ok, Value} ->
           {ok, Value};
      _Else ->
           {ok, Default}
  end.

set_value(App, Key, Value) when is_atom(App), is_atom(Key) ->
  case  application:set_env(App, Key, Value) of
      ok ->  ok;
      {ok , _Val} -> ok;
      {error, Error} -> {error, Error}
  end.


required(App, Key) when is_atom(Key) ->
  fun(ConfigKV)->
	%get from in memory or else from config
	Val = case proplists:get_value(Key, ConfigKV) of
		 undefined -> 
			case get_value(App, Key) of
                             {ok, Value0} -> Value0;                       
			     undefined -> undefined
 	                end;
		 Value -> Value
              end, 
        case Val of
             undefined -> undefined;
             AllVal -> {Key, AllVal}
        end           
  end.

optional(App, Key, Default) when is_atom(Key) ->
  fun(ConfigKV)->
	%get from in memory or else from config
	Val = case proplists:get_value(Key, ConfigKV, Default) of
		 {ok, Value} -> Value; 
	         _Else -> 
                       case get_value(App, Key, Default) of
                            {ok, Value0} -> Value0;
                            _Else -> Default
                       end   
              end, 
        {Key, Val}
  end.
 
read_config(Config, Config_Spec) when is_function(Config_Spec) ->
    [ F(Config) || F <- Config_Spec].

%% =========== unit tests ===========
-ifdef(TEST).

assert_app_running_state_test()->
  {inorder,
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
        {"Ensure app start from clean", fun app_should_clean_start/0},
        {"Ensure starting started, app remain started", fun app_already_start/0},
        {"Ensure starting undefined app complains", fun app_start_undef_app/0},
        {"Ensure stopping started app stops success", fun app_stop/0},
        {"Ensure stopping stopped app has no effect", fun app_already_stop/0},
        {"Ensure stopping undefined app has no effect", fun app_stop_undef_app/0}
      ]
    }
  }.


setup() ->
 ok.

cleanup(_State) -> 
 ok.

app_should_clean_start() ->
  ok = ensure_app_stop(sasl),  
  Ret = ensure_app_start(sasl),
  ?assertMatch(ok, Ret).
  
app_already_start()->
  ok = ensure_app_stop(sasl),
  ok = ensure_app_start(sasl),
  Ret1 = ensure_app_start(sasl),
  ?assertMatch(ok, Ret1).
 
app_start_undef_app()->
  Ret = ensure_app_start(undefined),
  io:format("Return value: ~p",[Ret]),
  {error, {"no such file or directory", _App}} = Ret.
  % ?assert(ok = ({error,_} = Ret)).
  % ?assertError({"no such file or directory","undefined.app"}, Ret).
 
app_stop()->
  ok = ensure_app_start(sasl),
  Ret0 = ensure_app_stop(sasl),
  ?assertEqual(ok, Ret0).
 
app_already_stop()->
  ok = ensure_app_start(sasl),
  ok = ensure_app_stop(sasl),
  Ret1 = ensure_app_stop(sasl),
  ?assertEqual(ok, Ret1).

%TODO behavirou not symmetrical with start. 
app_stop_undef_app()->
  Ret = ensure_app_stop(undefined),
  ?assertMatch(ok, Ret).

app_get_set_key_test_()->
 {inorder,
  { setup,
    fun setup/0,
    fun cleanup/1,
    [
      {"Setting application key", fun set_appvalue/0},
      {"Setting undefined application key", fun set_appvalue_undef_key/0},
      {"Setting key on undefined application", fun set_appvalue_undef_app/0},
      {"Get all app keys values", fun get_all_values_from_memory/0},
      {"Get one app key value", fun get_value_from_memory/0},
      {"Get key values form undefined app", fun get_value_undef_app/0},
      {"Get value on undefined key", fun get_value_undef_key/0},
      {"Get value or default on undefined key", fun get_value_or_default_undef_key/0}
    ]
  }}.

set_appvalue()->
  Ret = set_value(testapp, testkey, test_val),
  ?assertEqual(ok, Ret),
  Ret0 = application:get_env(testapp,testkey),
  ?assertMatch({ok,test_val},Ret0).
 
set_appvalue_undef_key()->
  Ret = set_value(testapp, undefined, test_val),
  % TODO this has to deal with becasue it allows key namesd as undefined 
  %?assertEqual(undefined, Ret).
  io:format("You can name a key undefined with even undefined as value."),
  ?assertEqual(ok, Ret).
 
set_appvalue_undef_app()->
  Ret = set_value(undefined,undefined,test),
  io:format("You can actually make an application with name undefined, key undefined"),
  ?assertEqual(ok ,Ret).
 
get_all_values_from_memory()->
  ok = set_value(testapp2, testkey, testvalue),
  ok = set_value(testapp2, testkey1, testvalue1),
  Ret = lists:reverse(get_all_value(testapp2)),
  ?assertMatch([{testkey, testvalue}, {testkey1, testvalue1}],
              Ret).
 
get_value_from_memory()->
  ok = set_value(testapp1,testkey1,testvalue),
  Ret = get_value(testapp1, testkey1),
  ?assertMatch({ok,testvalue}, Ret).
 
get_value_undef_app()->
  Ret = get_value(undefined,somekey),
  ?assertEqual(undefined, Ret) .
 
get_value_undef_key()-> 
 Ret = get_value(testapp,somekey),
 ?assertEqual(undefined, Ret) .

get_value_or_default_undef_key()->
  Ret = get_value(testapp,keyfake,defaultVal),
  ?assertMatch({ok,defaultVal}, Ret).

optional_required_key_test_()->
 { inorder,
   { setup, fun setupConfigSet/0, fun cleanup/1,
     [
      {"Ensure getting required in memory key value", 
	fun required_key_from_memory/0},
      {"Ensure getting required key from app env", 
	fun required_key_from_env/0},
      {"Ensure required in memory undefined key is undefined", 
	fun required_undefine_key_from_memory/0},
      {"Ensure required no value key is undefined", 
	fun required_key_no_val_from_memory/0},
      {"Ensure option in memory key is default value", 
	fun optional_key_from_memory/0},
      {"Ensure optional key from app env is default value", 
	fun optional_key_from_env/0},
      {"Ensure optional key from undefined app is default ", 
	fun optional_key_from_undefined_app/0}
     ]
   }
 }.

setupConfigSet()->
 ok.

required_key_from_memory()->
  App = tedtAppReq,
  InMemoryConfig = [{testAppReqKey, value2}],
  Ret = required(App, testAppReqKey),
  ?assertEqual({testAppReqKey, value2}, Ret(InMemoryConfig)).

required_undefine_key_from_memory()->
  App = testAppReq,
  InMemoryConfig = [{testAppReqKey, value2}],
  Ret = required(App, wrongKey),
  ?assertEqual(undefined, Ret(InMemoryConfig) ).

required_key_no_val_from_memory()->
  %App = testAppReq,
  InMemoryConfig = [{testAppReqKey, value2}],
  Ret = required(key1, testAppReqKey1),
  ?assertEqual(undefined, Ret(InMemoryConfig)).

required_key_from_env()->
  ok = application:set_env(testtest, testkey, testval),
  InMemoryConfig = [{testAppReqKey, value2}],
  Ret = required(testtest, testkey),
  ?assertEqual({testkey, testval}, Ret(InMemoryConfig)).

optional_key_from_memory()->
  App = testAppOpt0,
  InMemory = [{test, keytest}],
  Ret = optional(App, testkeyOpt0, defaultVal),
  ?assertEqual({testkeyOpt0, defaultVal}, Ret(InMemory)). 

optional_key_from_env()->
  App = testAppOpt1,
  InMemory = [{test, keytest}],
  ok = application:set_env(App, testkeyOpt1, testval1),
  Ret = optional(App, testkeyOpt2, defaultOptVal1),
  ?assertEqual({testkeyOpt2, defaultOptVal1}, Ret(InMemory)). 

optional_key_from_undefined_app()->
  App = undefinedApp,
  ok = application:set_env(App, testkeyOpt1, testval1),
  InMemory = [{test, keytest}],
  Ret = optional(App, testKeyOpt1, defaultOptVal1),
  ?assertMatch({testKeyOpt1, defaultOptVal1}, Ret(InMemory)).


read_config_test_()->
  {inorder,
    {setup,
     fun config_setup/0,
     fun cleanup/1,
     [
       {"Read config with required and optional keys",    
          fun read_config_with_required_optional_config/0}
     ]
    }
  }.

config_setup()->
  App = config_app, 
  [required(App, reqKey), optional(App, optKey,defVal)].

-endif.
