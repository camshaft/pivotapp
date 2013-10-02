-module(pivotapp).
-include("pivotapp.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
  DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, pivotapp),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, ping, pivotapp_vnode_master).

track(Env, App, Event, User) ->
  DocIdx = riak_core_util:chash_key({App, Event}),
  [{Node, _Type}] = riak_core_apl:get_primary_apl(DocIdx, 1, pivotapp_track),
  pivotapp_track_vnode:track(Node, Env, App, Event, User).

% internal