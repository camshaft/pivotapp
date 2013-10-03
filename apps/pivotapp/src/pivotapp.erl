-module(pivotapp).
-include("pivotapp.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0]).
-export([assign/3]).
-export([assign/4]).
-export([event/4]).
-export([reward/5]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
  DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, pivotapp),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, ping, pivotapp_vnode_master).

assign(Env, App, User) ->
  assign(Env, App, undefined, User).

assign(Env, App, AppVersion, User) ->
  DocIdx = riak_core_util:chash_key({App, User}),
  [Node] = riak_core_apl:get_apl(DocIdx, 1, pivotapp_assign),
  pivotapp_assign_vnode:assign(Node, Env, App, AppVersion, User).

event(Env, App, Event, User) ->
  DocIdx = riak_core_util:chash_key({App, Event}),
  [Node] = riak_core_apl:get_apl(DocIdx, 1, pivotapp_event),
  pivotapp_event_vnode:handle(Node, Env, App, Event, User).

reward(Env, App, Bandit, Arm, Reward) ->
  DocIdx = riak_core_util:chash_key({App, Bandit}),
  [Node] = riak_core_apl:get_apl(DocIdx, 1, pivotapp_state),
  pivotapp_state_vnode:reward(Node, Env, App, Bandit, Arm, Reward).

% internal
