-module(pivotapp_test_config_db).

-export([init/3]).
-export([get/3]).
-export([set/5]).

init(_App, _Bandit, _State) ->
  ok.

set(_Env, _App, _Bandit, _State, _Rewards) ->
  ok.

get(_Env, _App, _Bandit) ->
  {ok, []}.
