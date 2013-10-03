-module(pivotapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  require([ref_server]),

  ok = riakou:start(),
  RiakURL = simple_env:get_binary("RIAK_URL", <<"riak://localhost">>),
  Min = simple_env:get_integer("RIAK_POOL_MIN", 5),
  Max = simple_env:get_integer("RIAK_POOL_MAX", 50),
  ok = riakou:start_link(RiakURL, [], Min, Max),
  ok = riakou:wait_for_connection(),

  DB = pivotapp,
  pivotapp_ref:set(user, pivotapp_test_user_db),
  pivotapp_ref:set(arms, DB),
  pivotapp_ref:set(state, pivotapp_test_state_db),
  pivotapp_ref:set(config, pivotapp_test_config_db),
  pivotapp_ref:set(event, pivot_event_db_riak),
  pivotapp_ref:set(app, DB),

  case pivotapp_sup:start_link() of
    {ok, Pid} ->
      ok = riak_core:register([{vnode_module, pivotapp_vnode}]),
      ok = riak_core:register([{vnode_module, pivotapp_event_vnode}]),
      ok = riak_core:register([{vnode_module, pivotapp_state_vnode}]),

      ok = riak_core_ring_events:add_guarded_handler(pivotapp_ring_event_handler, []),
      ok = riak_core_node_watcher_events:add_guarded_handler(pivotapp_node_event_handler, []),

      ok = riak_core_node_watcher:service_up(pivotapp, self()),
      ok = riak_core_node_watcher:service_up(pivotapp_event, self()),
      ok = riak_core_node_watcher:service_up(pivotapp_state, self()),

      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

stop(_State) ->
  ok.

%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
  ok;
require([App|Tail]) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end,
  require(Tail).
