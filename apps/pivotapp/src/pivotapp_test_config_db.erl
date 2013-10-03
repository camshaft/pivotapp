-module(pivotapp_test_config_db).

-export([init/3]).
-export([get/3]).
-export([set/4]).

init(_App, _Bandit, _State) ->
  ok.

set(_Env, _App, _Bandit, _Config) ->
  ok.

get(_Env, _App, _Bandit) ->
  {ok, []}.
