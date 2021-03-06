-module(pivot_mab_ucb1).

-export([init/1]).
-export([select/2]).
-export([update/4]).
-export([diff/3]).
-export([report/2]).

-spec init(Opts)
    -> {ok, pivot_mab:config()}
    | {error, any()}
    when Opts::pivot_mab:options().
init(_Opts) ->
  {ok, []}.

-spec select(State, Config)
    -> {ok, pivot_mab:arm(), pivot_mab:state()}
    | {error, any()}
    when State::pivot_mab:state(), Config::pivot_mab:config().
select(State, Config) ->
  maybe_prepare_state(State, 0, State, lists:member(explore, Config)).

% Return arms that haven't been tried yet and total the number of arms
maybe_prepare_state([{Arm, {0, _Score}}|_Arms], _TotalCount, State, _) ->
  {ok, Arm, State};
maybe_prepare_state([{_Arm, {Count, _Score}}|Arms], TotalCount, State, Explore) ->
  maybe_prepare_state(Arms, TotalCount+Count, State, Explore);
maybe_prepare_state([], TotalCount, State, Explore) ->
  choose_best_arm(TotalCount, {undefined, -1}, State, State, Explore).

% Iterate a list of arms and pick the highest
choose_best_arm(TotalCount, {_HighArm, HighScore} = High, [{Arm, {Count, Score}}|Arms], State, Explore) ->
  case calculate_score(TotalCount, Count, Score, Explore) of
    NewScore when NewScore > HighScore ->
      choose_best_arm(TotalCount, {Arm, NewScore}, Arms, State, Explore);
    _ ->
      choose_best_arm(TotalCount, High, Arms, State, Explore)
  end;
choose_best_arm(_, {HighArm, _}, [], State, _) ->
  {ok, HighArm, State}.

calculate_score(TotalCount, Count, Score, true) ->
  Score + math:sqrt((2 * math:log(TotalCount)) / Count);
calculate_score(TotalCount, Count, Score, _) ->
  Score + math:sqrt((math:log(TotalCount)) / Count).

-spec update(Arm, Reward, State, Config)
    -> {ok, State}
    | {error, any()}
    when Arm::pivot_mab:arm(), Reward::pivot_mab:reward(), State::pivot_mab:state(), Config::pivot_mab:config().
update(ArmName, Reward, State, _Config) when Reward > 0.0 orelse Reward < 1.0 ->
  {ok, update_score(ArmName, Reward, State, [])}.

update_score(ArmName, Reward, [], Acc) ->
  lists:reverse([{ArmName, {1, Reward}}|Acc]);
update_score(ArmName, Reward, [{ArmName, {Count, Score}}|State], Acc) ->
  N = Count + 1,
  NewScore = ((N - 1.0) / N) * Score + (1.0 / N) * Reward,
  lists:reverse(Acc) ++ [{ArmName, {N, NewScore}}|State];
update_score(ArmName, Reward, [Arm|State], Acc) ->
  update_score(ArmName, Reward, State, [Arm|Acc]).

diff(OldState, NewState, _Config) ->
  {ok, do_diff(OldState, NewState, [])}.

do_diff([], [], Acc) ->
  Acc;
do_diff([Arm|OldState], [Arm|NewState], Acc) ->
  do_diff(OldState, NewState, Acc);
do_diff([{Arm, {OldCount, OldScore}}|OldState], [{Arm, {NewCount, NewScore}}|NewState], Acc) ->
  do_diff(OldState, NewState, [{Arm, {NewCount - OldCount, NewScore - OldScore}}|Acc]).

report(State, _Config) ->
  TotalCount = count(State, 0),
  {ok, TotalCount, [{Arm, calculate_score(TotalCount, Count, Score, true)} || {Arm, {Count, Score}} <- State]}.

count([], TotalCount) ->
  TotalCount;
count([{_Arm, {Count, _Score}}|Arms], TotalCount) ->
  count(Arms, Count + TotalCount).
