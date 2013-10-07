-module(pivotapp_api).

-export([start/0]).

%% API.

start() ->
  ok = inets:start(),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(pivotapp),
  ok = application:start(pivotapp_api).
