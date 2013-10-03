-module(pivotapp_test_user_db).

-export([assignments/3]).
-export([increment_usage/5]).
-export([assign/6]).
-export([unassign/5]).

assignments(_, _, _) ->
  {ok, [
    {<<"bandit-1">>, <<"arm1">>, 1, 9999999999},
    {<<"bandit-2">>, <<"arm3">>, 1, 9999999999},
    {<<"expired">>, <<"arm1">>, 1, 123}
  ]}.

increment_usage(_Env, _App, _User, _Bandit, _Arm) ->
  ok.

assign(_Env, _App, _User, _Bandit, _Arm, _Expiration) ->
  ok.

unassign(_Env, _App, _User, _Bandit, _Arm) ->
  ok.
