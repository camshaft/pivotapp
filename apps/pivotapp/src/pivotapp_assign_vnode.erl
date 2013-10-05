-module(pivotapp_assign_vnode).
-behaviour(riak_core_vnode).
-include("pivotapp.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-export([assign/5]).

-record(state, {
  partition,
  ref
}).

-record(req, {
  env,
  app,
  app_version,
  user,
  current_assignments,
  assignments = [],
  expiration = 0,
  enabled_set,
  enabled,
  now
}).

-define(DEFAULT_TTL, 86400).

%% API
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

assign(Node, Env, App, AppVersion, User) ->
  riak_core_vnode_master:sync_command(Node, {assign, Env, App, AppVersion, User}, pivotapp_assign_vnode_master).

%% Callbacks
init([Partition]) ->
  {ok, #state { partition=Partition, ref=pivotapp_ref:get() }}.

handle_command({assign, Env, App, AppVersion, User}, _Sender, State = #state{ ref=Ref }) ->
  Response = handle_assignment(#req{
    env=Env,
    app=App,
    app_version=AppVersion,
    user=User,
    now=pivotapp_clock:time()
  }, Ref),
  {reply, Response, State}.
 
handle_handoff_command(_Message, _Sender, State) ->
  {noreply, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

handle_handoff_data(_Data, State) ->
  {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
  <<>>.

is_empty(State) ->
  {true, State}.

delete(State) ->
  {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% internal
handle_assignment(Req = #req{ app_version=undefined }, Ref) ->
  lookup_enabled_bandits(Req, Ref);
handle_assignment(Req, Ref) ->
  validate_app_version(Req, Ref).

%% TODO
validate_app_version(Req, Ref) ->
  lookup_enabled_bandits(Req, Ref).

lookup_enabled_bandits(Req = #req{ env=Env, app=App }, Ref) ->
  AppDB = pivotapp_ref:app(Ref),
  case AppDB:enabled(Env, App) of
    {ok, Enabled} ->
      lookup_assignment(Req#req{ enabled_set=gb_sets:from_list(Enabled), enabled=Enabled }, Ref);
    Error ->
      Error
  end.

lookup_assignment(Req = #req{ env=Env, app=App, user=User }, Ref) ->
  UserDB = pivotapp_ref:user(Ref),
  case UserDB:assignments(Env, App, User) of
    {ok, []} ->
      pick_bandit(Req, Ref);
    {ok, Assignments} ->
      validate_assignments(Assignments, Req, Ref);
    _ ->
      pick_bandit(Req, Ref)
  end.

validate_assignments([], Req = #req{ assignments=[] }, Ref) ->
  pick_bandit(Req, Ref);
validate_assignments([], #req{ assignments=Assignments, expiration=Expiration, now=Now }, _Ref) ->
  {ok, Assignments, Expiration - Now};
validate_assignments([{_Bandit, _Arm, _Count, Expiration}|Assignments], Req = #req{ now=Now }, Ref) when is_integer(Expiration), Now > Expiration ->
  %% TODO unassign
  %% TODO penalize the bandit/arm if the count is 0
  validate_assignments(Assignments, Req, Ref);
validate_assignments([{Bandit, Arm, _Count, _Expiration}|Assignments], Req = #req{ env=Env, app=App, enabled_set=Enabled, assignments=ValidAssignments }, Ref) ->
  case gb_sets:is_member(Bandit, Enabled) of
    false ->
      validate_assignments(Assignments, Req, Ref);
    _ ->
      ArmsDB = pivotapp_ref:arms(Ref),
      {ok, EnabledArms} = ArmsDB:enabled(Env, App, Bandit),
      case gb_sets:is_member(Arm, gb_sets:from_list(EnabledArms)) of
        false ->
          validate_assignments(Assignments, Req, Ref);
        _ ->
          %% TODO set the smaller expiration
          validate_assignments(Assignments, Req#req{ assignments=[{Bandit, Arm}|ValidAssignments] }, Ref)
      end
  end.

pick_bandit(Req = #req{ env=Env, app=App, enabled=EnabledBandits, now=Now, user=User }, Ref) ->
  {ok, ExplorationBandit} = select_arm(?SUPER_BANDIT, EnabledBandits, Req, Ref, true),
  ArmsDB = pivotapp_ref:arms(Ref),
  UserDB = pivotapp_ref:user(Ref),

  Expiration = Now + ?DEFAULT_TTL,

  Assignments = [begin
    {ok, EnabledArms} = ArmsDB:enabled(Env, App, Bandit),
    {ok, Arm} = select_arm(Bandit, EnabledArms, Req, Ref, ExplorationBandit =:= Bandit),

    ok = UserDB:assign(Env, App, User, Bandit, Arm, Expiration),

    {Bandit, Arm}
  end || Bandit <- EnabledBandits],

  {ok, Assignments, Expiration}.

select_arm(Bandit, EnabledArms, Req, Ref, true) ->
  select_arm(Bandit, EnabledArms, Req, Ref, [explore]);
select_arm(Bandit, EnabledArms, Req, Ref, false) ->
  select_arm(Bandit, EnabledArms, Req, Ref, []);
select_arm(Bandit, EnabledArms, #req{ env=Env, app=App }, Ref, Options) ->
  StateDB = pivotapp_ref:state(Ref),
  ConfigDB = pivotapp_ref:config(Ref),

  {ok, MabAlgo, MabState, _} = StateDB:get(Env, App, Bandit),
  {ok, Config} = ConfigDB:get(Env, App, Bandit),

  FilteredState = [{Arm, fast_key:get(Arm, MabState, {0, 0.0})} || Arm <- EnabledArms],
  {ok, SelectedArm, _} = MabAlgo:select(FilteredState, Config ++ Options),
  {ok, SelectedArm}.
