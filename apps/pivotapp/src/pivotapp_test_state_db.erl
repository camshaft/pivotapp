-module(pivotapp_test_state_db).

-export([init/3]).
-export([get/3]).
-export([set/6]).

init(_App, _Bandit, _State) ->
  ok.

set(_Env, _App, _Bandit, _State, _Rewards, _Version) ->
  io:format("~p~n", [_State]),
  ok.

get(_Env, _App, _Bandit) ->
  {ok, pivot_mab_ucb1, [
    {<<"arm1">>, {100, 80.0}},
    {<<"arm2">>, {90, 60.0}},
    {<<"arm3">>, {60, 5.0}}
  ], riak_obj}.
