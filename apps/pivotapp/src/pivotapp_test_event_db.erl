-module(pivotapp_test_event_db).

-export([get/3]).

get(_Env, _App, <<"purchase">>) ->
  {ok, 1.0};
get(_Env, _App, <<"add-to-cart">>) ->
  {ok, 0.8};
get(_Env, _App, <<"view-item">>) ->
  {ok, 0.6};
get(_Env, _App, <<"leave-site">>) ->
  {ok, 0.0};
get(_, _, _) ->
  {error, notfound}.
