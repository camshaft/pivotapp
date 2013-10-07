-module(pivotapp_api_assignment).

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
  {UserID, Req3} = cowboy_req:qs_val(<<"u">>, Req2),
  maybed_assign(App, UserID, Req3);
check_method(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

maybed_assign(undefined, _, Req) ->
  ?ERROR(<<"Missing app (a) parameter.">>, Req);
maybed_assign(_, undefined, Req) ->
  ?ERROR(<<"Missing user id (u) parameter.">>, Req);
maybed_assign(App, UserID, Req) ->
  Res = pivotapp:assign(cowboy_env:get(Req), App, UserID),
  respond(Res, Req).

respond({ok, Assignments, CacheLength}, Req) ->
  Now = pivotapp_clock:time(),
  Body = jsx:encode([
    {<<"assignments">>, Assignments},
    {<<"expiration">>, Now}
  ]),
  cowboy_req:reply(200, [{<<"cache-control">>, <<"max-age=", (integer_to_binary(CacheLength - Now))/binary>>}], Body, Req);

respond({ok, Assignments}, Req) ->
  Body = jsx:encode([
    {<<"assignments">>, Assignments}
  ]),
  cowboy_req:reply(200, [], Body, Req);
respond(_, Req) ->
  cowboy_req:reply(404, [], <<>>, Req).

terminate(_Reason, _Req, _State) ->
  ok.
