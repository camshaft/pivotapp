-module(pivotapp_api_root).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("pivotapp_api.hrl").

init(_Transport, Req, _) ->
  {ok, Req, ok}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, Req3} = check_method(Method, Req2),
  {ok, Req3, State}.

check_method(<<"GET">>, Req) ->
  {App, Req2} = cowboy_req:qs_val(<<"a">>, Req),
  {Event, Req3} = cowboy_req:qs_val(<<"e">>, Req2),
  {UserID, Req4} = cowboy_req:qs_val(<<"u">>, Req3),
  maybe_track(App, Event, UserID, Req4);
check_method(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

maybe_track(undefined, _, _, Req) ->
  ?ERROR(<<"Missing app (a) parameter.">>, Req);
maybe_track(_, undefined, _, Req) ->
  ?ERROR(<<"Missing event (e) parameter.">>, Req);
maybe_track(_, _, undefined, Req) ->
  ?ERROR(<<"Missing user id (u) parameter.">>, Req);
maybe_track(App, Event, UserID, Req) ->
  ok = pivotapp:event(cowboy_env:get(Req), App, Event, UserID),
  cowboy_req:reply(200, [
    {<<"cache-control">>, <<"no-cache, max-age=0">>}
  ], <<>>, Req).

terminate(_Reason, _Req, _State) ->
  ok.
