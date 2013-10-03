-module(pivotapp_test_user_db).

-export([assignments/3]).
-export([increment_usage/5]).
-export([assign/5]).
-export([unassign/5]).

assignments(_, _, _) ->
  {ok, [{<<"bandit-1">>, <<"arm1">>, 1}]}.

increment_usage(_Env, _App, _User, _Bandit, _Arm) ->
  ok.

assign(_Env, _App, _User, _Bandit, _Arm) ->
  ok.

unassign(_Emv, _App, _User, _Bandit, _Arm) ->
  ok.
