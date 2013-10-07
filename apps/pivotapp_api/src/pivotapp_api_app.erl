-module(pivotapp_api_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  {ok, _} = cowboy:start_http(http, simple_env:get_integer("NUM_LISTENERS", 100), [
    {port, Port = simple_env:get_integer("PORT", 5000)}
  ], [
    {compress, true},
    {env, [
      {dispatch, cowboy_route_loader:compile(pivotapp_api)}
    ]},
    {middlewares, [
      cowboy_env,
      cowboy_router,
      cowboy_handler
    ]}
  ]),

  io:format("Server started on port ~p\n", [Port]),
  simple_sup:start_link([]).

stop(_State) ->
  ok.
