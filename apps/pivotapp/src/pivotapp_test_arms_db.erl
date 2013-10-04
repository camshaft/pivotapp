-module(pivotapp_test_arms_db).

-export([start_link/0]).

-export([add/4]).
-export([remove/4]).
-export([enable/4]).
-export([disable/4]).
-export([all/3]).
-export([enabled/3]).

start_link() ->
  ?MODULE = ets:new(?MODULE, [public, named_table, duplicate_bag]).

add(Env, App, Bandit, Arm) ->
  enable(Env, App, Bandit, Arm).

remove(_Env, _App, _Bandit, _Arm) ->
  ok.

enable(Env, App, Bandit, Arm) ->
  true = ets:insert(?MODULE, {{Env, App, Bandit}, Arm}),
  ok.

disable(_Env, _App, _Bandit, _Arm) ->
  ok.

all(Env, App, Bandit) ->
  try ets:lookup_element(?MODULE, {Env, App, Bandit}, 2) of
    Arms ->
      {ok, Arms}
  catch
    _:_ ->
      {ok, []}
  end.

enabled(Env, App, Bandit) ->
  all(Env, App, Bandit).
