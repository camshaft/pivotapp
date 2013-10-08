-module(pivotapp_state_vnode).
-behaviour(riak_core_vnode).
-include("pivotapp.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         handle_info/2,
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

-export([reward/6]).

%% private
-export([update/3]).

-record(state, {
  partition,
  buffer,
  ref
}).

%% API
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

reward(Node, Env, App, Bandit, Arm, Reward) ->
  riak_core_vnode_master:command(Node, {reward, Env, App, Bandit, Arm, Reward}, pivotapp_state_vnode_master).

%% Callbacks
init([Partition]) ->
  timer:send_interval(1000, flush),
  {ok, #state { partition=Partition, buffer=buffer(), ref=pivotapp_ref:get() }}.

handle_command({reward, Env, App, Bandit, Arm, Reward}, _Sender, State = #state{buffer = Buffer}) ->
  true = ets:insert(Buffer, {{Env, App, Bandit}, {Arm, Reward}}),
  {noreply, State}.

handle_info(flush, State = #state{ buffer=Buffer, ref=Ref }) ->
  case buffer_empty(State) of
    true ->
      {ok, State};
    _ ->
      spawn_link(?MODULE, update, [Buffer, pivotapp_ref:state(Ref), pivotapp_ref:config(Ref)]),
      {ok, State#state{buffer = buffer()}}
  end.

handle_handoff_command(_Message, _Sender, State) ->
  {noreply, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

handle_handoff_data(Data, State = #state{ buffer=Buffer }) ->
  {Key, Value} = binary_to_term(Data),
  true = ets:insert(Buffer, Key, Value),
  {reply, ok, State}.

encode_handoff_item(Key, Value) ->
  term_to_binary({Key, Value}).

is_empty(State) ->
  {buffer_empty(State), State}.

delete(State) ->
  {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% private
buffer_empty(#state{buffer = Buffer}) ->
  ets:info(Buffer, size) =:= 0.

buffer() ->
  ets:new(?MODULE, [duplicate_bag, public, {read_concurrency, true}]).


%% update the state for the bandits
update(Buffer, StateDB, ConfigDB) ->
  ok = update_bandit(ets:first(Buffer), Buffer, StateDB, ConfigDB),
  true = ets:delete(Buffer),
  ok.

update_bandit('$end_of_table', _, _, _) ->
  ok;
update_bandit(Key = {Env, App, Bandit}, Buffer, StateDB, ConfigDB) ->
  %% TODO could we parallelize this?
  {ok, MabAlgo, MabState, Version} = StateDB:get(Env, App, Bandit),
  {ok, Config} = ConfigDB:get(Env, App, Bandit),
  Rewards = ets:lookup_element(Buffer, Key, 2),

  {ok, NewMabState} = update_arm(Rewards, MabAlgo, MabState, Config),
  ok = StateDB:set(Env, App, Bandit, NewMabState, Rewards, Version),

  update_bandit(ets:next(Buffer, Key), Buffer, StateDB, ConfigDB).

update_arm([], _MabAlgo, MabState, _Config) ->
  {ok, MabState};
update_arm([{Arm, Reward}|Rewards], MabAlgo, MabState, Config) ->
  {ok, NewState} = MabAlgo:update(Arm, Reward, MabState, Config),
  update_arm(Rewards, MabAlgo, NewState, Config).
