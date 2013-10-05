-module(pivotapp_test_user).

-export([run/1]).
-export([start/1]).
-export([weight/3]).

-define(NUM_USERS, 999999999999999).

run(Max) ->
  run(Max, 0).

run(Max, Count) when Count =:= Max ->
  ok;
run(Max, Count) when 0 =:= Max rem 5 ->
  ID = user_id(),
  wait(ID),
  spawn_link(?MODULE, start, [ID]),
  run(Max, Count + 1);
run(Max, Count) ->
  ID = user_id(),
  spawn_link(?MODULE, start, [ID]),
  run(Max, Count + 1).

user_id() ->
  <<"user-", (integer_to_binary(crypto:rand_uniform(0, ?NUM_USERS)))/binary>>.

start(ID) ->
  wait(ID),
  {ok, Assignments, _Expiration} = pivotapp:assign(<<"env">>, <<"app">>, ID),
  % io:format("~p\t\t~p~n", [ID, Assignments]),
  [begin
    wait(ID),
    ok = pivotapp:event(<<"env">>, <<"app">>, events(Assignments), ID)
  end || _ <- lists:seq(0, crypto:rand_uniform(3, 10))].

wait(_ID)->
  Time = crypto:rand_uniform(0, 500),
  timer:sleep(Time).

events(Assignments) ->
  % io:format("Assignments ~p~n", [Assignments]),
  Events = [weight(events(Bandit, Arm)) || {Bandit, Arm} <- Assignments],
  % io:format("Events ~p~n", [Events]),
  Event = lists:nth(crypto:rand_uniform(1, length(Events)+1), Events),
  % io:format("Event ~p~n", [Event]),
  Event.

events(<<"button-color">>, <<"red">>) ->
  [
    {<<"purchase">>, 10},
    {<<"add-to-cart">>, 20},
    {<<"leave-site">>, 50},
    {<<"view-item">>, 10}
  ];
events(<<"button-color">>, <<"blue">>) ->
  [
    {<<"purchase">>, 10},
    {<<"add-to-cart">>, 50},
    {<<"view-item">>, 10},
    {<<"leave-site">>, 30}
  ];
events(<<"button-color">>, <<"green">>) ->
  [
    {<<"purchase">>, 0},
    {<<"add-to-cart">>, 10},
    {<<"leave-site">>, 60},
    {<<"view-item">>, 30}
  ];
events(<<"button-color">>, <<"yellow">>) ->
  [
    {<<"purchase">>, 0},
    {<<"add-to-cart">>, 10},
    {<<"leave-site">>, 70},
    {<<"view-item">>, 20}
  ];
events(<<"logo">>, <<"big">>) ->
  [
    {<<"purchase">>, 50},
    {<<"add-to-cart">>, 0},
    {<<"leave-site">>, 30},
    {<<"view-item">>, 20}
  ];
events(<<"logo">>, <<"medium">>) ->
  [
    {<<"purchase">>, 5},
    {<<"add-to-cart">>, 15},
    {<<"leave-site">>, 50},
    {<<"view-item">>, 30}
  ];
events(<<"logo">>, <<"small">>) ->
  [
    {<<"purchase">>, 20},
    {<<"add-to-cart">>, 10},
    {<<"leave-site">>, 50},
    {<<"view-item">>, 20}
  ].

weight(Events) ->
  Bucket = crypto:rand_uniform(0, 100),
  weight(Bucket, Events, 0).

weight(_, [{Event, _}], _) ->
  Event;
weight(Bucket, [{Event, Weight}|_], Idx) when Bucket >= Idx, Bucket < Idx + Weight ->
  Event;
weight(Bucket, [{_, Weight}|Events], Idx) ->
  weight(Bucket, Events, Idx + Weight).
