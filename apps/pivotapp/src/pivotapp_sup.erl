-module(pivotapp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
  Procs = [
    vnode(pivotapp_vnode, pivotapp_vnode_master),
    vnode(pivotapp_assign_vnode, pivotapp_assign_vnode_master),
    vnode(pivotapp_event_vnode, pivotapp_event_vnode_master),
    vnode(pivotapp_state_vnode, pivotapp_state_vnode_master),
    {
      pivotapp_clock, {
        pivotapp_clock, start_link, []
      }, permanent, 5000, worker, [pivotapp_clock]
    }
  ],

  {ok, {{one_for_one, 5, 10}, Procs}}.

vnode(Name, Master) ->
  {
    Master, {
      riak_core_vnode_master, start_link, [Name]
    }, permanent, 5000, worker, [Master]
  }.
