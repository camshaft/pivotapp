-module(pivotapp_event_vnode).
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

-export([handle/5]).

-record(state, {
  partition,
  ref
}).

-record(event, {
  env,
  app,
  user,
  reward,
  cardinality
}).

-define(SUPER_BANDIT, <<"$$super-bandit">>).

%% API
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

handle(Node, Env, App, Event, User) ->
  riak_core_vnode_master:command(Node, {event, Env, App, Event, User}, pivotapp_event_vnode_master).

%% Callbacks
init([Partition]) ->
  {ok, #state { partition=Partition, ref=pivotapp_ref:get() }}.

handle_command({event, Env, App, Event, User}, _Sender, State = #state{ ref=Ref }) ->
  EventDB = pivotapp_ref:event(Ref),
  handle_event(EventDB:get(Env, App, Event), #event{env = Env, app = App, user = User}, Ref),
  {noreply, State}.

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
handle_event({ok, Reward, Cardinality}, Event, Ref) ->
  lookup_assignments(Event#event{ reward=Reward, cardinality=Cardinality }, Ref);
handle_event({ok, Reward}, Event, Ref) ->
  lookup_assignments(Event#event{ reward=Reward }, Ref);
handle_event({error, notfound}, _, _) ->
  ok.

lookup_assignments(Event = #event{ env=Env, app=App, user=User }, Ref) ->
  UserDB = pivotapp_ref:user(Ref),
  {ok, Assignments} = UserDB:assignments(Env, App, User),
  ok = maybe_reward(Assignments, UserDB, Event).

maybe_reward([{Bandit, Arm, Usages}|Assignments], UserDB, Event = #event{ env=Env, app=App, user=User, cardinality=Cardinality} ) when is_integer(Cardinality), Cardinality =< Usages ->
  %% remove the assignment from the user - it's expired
  ok = UserDB:unassign(Env, App, User, Bandit, Arm),
  ok = reward(Bandit, Arm, Event),
  maybe_reward(Assignments, UserDB, Event);
maybe_reward([{Bandit, Arm, _Usages}|Assignments], UserDB, Event = #event{ env=Env, app=App, user=User }) ->
  ok = UserDB:increment_usage(Env, App, User, Bandit, Arm),
  ok = reward(Bandit, Arm, Event),
  maybe_reward(Assignments, UserDB, Event);
maybe_reward([{Bandit, Arm}|Assignments], UserDB, Event) ->
  ok = reward(Bandit, Arm, Event),
  maybe_reward(Assignments, UserDB, Event);
maybe_reward([], _UserDB, _Event) ->
  ok.

reward(Bandit, Arm, #event{ env=Env, app=App, reward=Reward }) ->
  pivotapp:reward(Env, App, Bandit, Arm, Reward),
  pivotapp:reward(Env, App, ?SUPER_BANDIT, Bandit, Reward).
