-module(pivotapp_ref).

-export([get/0]).
-export([set/2]).

-export([
  user/1,
  arms/1,
  state/1,
  config/1,
  event/1,
  app/1
]).

-record(ref, {
  user,
  arms,
  state,
  config,
  event,
  app
}).

get() ->
  {ok, Ref} = ref_server:get(pivotapp, app, ref),
  Ref.

set(user, DB) ->
  Ref = get_or_create(),
  set(Ref#ref{user = DB});
set(arms, DB) ->
  Ref = get_or_create(),
  set(Ref#ref{arms = DB});
set(state, DB) ->
  Ref = get_or_create(),
  set(Ref#ref{state = DB});
set(config, DB) ->
  Ref = get_or_create(),
  set(Ref#ref{config = DB});
set(event, DB) ->
  Ref = get_or_create(),
  set(Ref#ref{event = DB});
set(app, DB) ->
  Ref = get_or_create(),
  set(Ref#ref{app = DB}).

get_or_create() ->
  case ref_server:get(pivotapp, app, ref) of
    {ok, Ref} ->
      Ref;
    _ ->
      #ref{}
  end.

set(Ref) ->
  ref_server:set(pivotapp, app, ref, Ref).

user(Ref) -> Ref#ref.user.
arms(Ref) -> Ref#ref.arms.
state(Ref) -> Ref#ref.state.
config(Ref) -> Ref#ref.config.
event(Ref) -> Ref#ref.event.
app(Ref) -> Ref#ref.app.