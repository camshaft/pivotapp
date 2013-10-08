-module(pivotapp_test_app_db).

-export([bandits/2]).
-export([enabled/2]).

enabled(Env, App) ->
  bandits(Env, App).

bandits(_Env, _App) ->
  {ok, [<<"button-color">>, <<"logo">>]}.
