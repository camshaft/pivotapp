-module(pivotapp_test_app_db).

-export([bandits/2]).
-export([enabled/2]).
-export([add/3]).

enabled(Env, App) ->
  bandits(Env, App).

bandits(_Env, _App) ->
  {ok, [<<"button-color">>, <<"logo">>]}.

add(_Env, _App, _Bandit) ->
  ok.
