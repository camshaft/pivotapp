-module(pivotapp_test_state_db).

-export([start_link/0]).

-export([init/4]).
-export([get/3]).
-export([set/6]).

start_link() ->
  ?MODULE = ets:new(?MODULE, [public, named_table]).

init(Env, App, Bandit, State) ->
  set(Env, App, Bandit, State, [], 1).

set(Env, App, Bandit, State, _Rewards, _Version) ->
  true = ets:insert(?MODULE, {{Env, App, Bandit}, State}),
  ok.

get(Env, App, Bandit) ->
  try ets:lookup_element(?MODULE, {Env, App, Bandit}, 2) of
    State ->
      {ok, pivot_mab_ucb1, State, 1}
  catch
    _:_ ->
      {ok, pivot_mab_ucb1, [], 1}
  end.
