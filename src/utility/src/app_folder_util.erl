-module(app_folder_util).
-export([cwd/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

cwd()->
    {ok, Dir} = file:get_cwd(),
    Dir.
    
    
-ifdef(TEST).
setup() -> ok.
cleanup(_Args) -> ok.
assert_app_running_state_test_()->
 {inorder,
  { setup,
    fun setup/0,
    fun cleanup/1,
    [
      {"Getting current working dir", fun get_current_working_dir/0}
    ]
  }}.

get_current_working_dir()->
   Dir = cwd(),
   ?assert(string:len(Dir) > 0).

-endif.
