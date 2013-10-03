-module(pivotapp_clock).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([time/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  timer
}).

-define(UPDATE, ets:insert(?MODULE, {time, to_unix(erlang:universaltime())})).

%% API.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

time() ->
  ets:lookup_element(?MODULE, time, 2).

stop() ->
  gen_server:call(?MODULE, stop).

%% gen_server.

init([]) ->
  ?MODULE = ets:new(?MODULE, [named_table, {read_concurrency, true}]),
  ?UPDATE,
  {ok, Timer} = timer:send_interval(1000, update),
  {ok, #state{ timer=Timer }}.

handle_call(stop, _, State = #state{ timer=Timer }) ->
  {ok, cancel} = timer:cancel(Timer),
  {stop, normal, stopped, State};
handle_call(_, _, State) ->
  {reply, ignore, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(update, State) ->
  ?UPDATE,
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal.
to_unix(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.
