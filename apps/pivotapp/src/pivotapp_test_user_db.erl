-module(pivotapp_test_user_db).

-export([start_link/0]).

-export([assignments/3]).
-export([increment_usage/5]).
-export([assign/6]).
-export([unassign/5]).

start_link() ->
  ?MODULE = ets:new(?MODULE, [public, named_table, duplicate_bag]).

assignments(Env, App, User) ->
  try ets:lookup_element(?MODULE, {Env, App, User}, 2) of
    Assignments ->
      {ok, Assignments}
  catch
    _:_ ->
      {ok, []}
  end.

increment_usage(_Env, _App, _User, _Bandit, _Arm) ->
  ok.

assign(Env, App, User, Bandit, Arm, Expiration) ->
  true = ets:insert(?MODULE, {{Env, App, User}, {Bandit, Arm, 0, Expiration}}),
  ok.

unassign(_Env, _App, _User, _Bandit, _Arm) ->
  ok.
