-module (app_util).

-author (edwardt.tril@gmail.com).

-export([ensure_app_start/1, 
         ensure_app_stop/1,
	 ensure_started/1,
	 ensure_stopped/1]).

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

%% =========== unit tests ===========
-ifdef(TEST).

assert_app_running_state_test_()->
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
        {"Ensure stopping undefined app has no effect", fun app_stop_undef_app/0},
        {"Ensure starting empty app list ok", fun app_start_empty_list/0},
        {"Ensure stopping empty app list ok", fun app_stop_empty_list/0}
      ]
    }
  }.

setup() ->
 ok.

cleanup(_State) -> 
 ok.

app_stop_empty_list()->
  ?assertMatch(ok, ensure_stopped([])).
  
app_start_empty_list()->
  ?assertMatch(ok, ensure_started([])).

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

-endif.
