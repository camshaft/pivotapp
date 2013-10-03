-module(pivotapp_test_arms_db).

-export([set/4]).
-export([all/3]).
-export([enabled/3]).

set(_Env, _App, _Bandit, _Arms) ->
  ok.

all(_Env, _App, _Bandit) ->
  enabled(_Env, _App, _Bandit).

enabled(_Env, _App, Bandit) ->
  {ok, [<<Bandit/binary, "-arm-1">>, <<Bandit/binary, "-arm-2">>, <<Bandit/binary, "-arm-3">>]}.
