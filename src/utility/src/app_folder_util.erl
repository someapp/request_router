-module(app_folder_util).
-export([cwd/0]).

cwd()->
    {ok, Dir} = file:get_cwd(),
    Dir.
