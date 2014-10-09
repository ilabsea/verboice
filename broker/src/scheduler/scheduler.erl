-module(scheduler).
-export([start_link/0, load/0, enqueue/1, verify_session/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SESSION_CLEANUP_TIME, 60 * 60 * 1000).

-record(state, {last_id, waiting_calls}).

-include("db.hrl").
-foo(something).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

load() ->
  gen_server:cast(?SERVER, load).

verify_session() ->
  gen_server:cast(?SERVER, verify_session).

clean_session() ->
  clean_session(supervisor:which_children(session_sup)).

clean_session([]) -> [];
clean_session([{_Id, Pid, _, _} | Rest]) ->
  terminate_session(Pid),
  clean_session(Rest).

terminate_session(SessionPid) ->
  session:no_ack(SessionPid).

enqueue(Call) ->
  gen_server:cast(?SERVER, {enqueue, Call}).

%% @private
init({}) ->
  % Initial load after 1 second
  timer:apply_after(timer:seconds(1), ?MODULE, load, []),

  % Load queued calls every 1 minute, just in case the broker misses a notification
  % about new queued calls.
  timer:apply_interval(timer:minutes(1), ?MODULE, load, []),

  % Check every 10 seconds for due calls
  timer:send_interval(timer:seconds(10), dispatch),

  % Remove stacked session that take longer than SESSION_CLEANUP_TIME
  timer:apply_interval(?SESSION_CLEANUP_TIME, ?MODULE, verify_session, []),

  {ok, #state{last_id = 0, waiting_calls = ordsets:new()}}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(verify_session, State)->
  clean_session(),
  {noreply, State};
handle_cast(load, State = #state{last_id = LastId}) ->
  LoadedCalls = queued_call:find_all([{call_log_id, '>', LastId}, {state, <<"queued">>}], [{order_by, not_before}]),
  NewState = lists:foldl(fun process_call/2, State, LoadedCalls),

  {noreply, NewState};

handle_cast({enqueue, Call}, State = #state{waiting_calls = WaitingCalls}) ->
  {noreply, State#state{waiting_calls = enqueue(Call, WaitingCalls)}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(dispatch, State = #state{waiting_calls = WaitingCalls}) ->
  WaitingCalls2 = dispatch(WaitingCalls),
  {noreply, State#state{waiting_calls = WaitingCalls2}};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

process_call(Call, State = #state{last_id = LastId, waiting_calls = WaitingCalls}) ->
  NewWaitingCalls = case Call:should_trigger() of
    true ->
      channel_queue:enqueue(Call),
      WaitingCalls;
    false ->
      enqueue(Call, WaitingCalls)
  end,
  State#state{last_id = max(Call#queued_call.call_log_id, LastId), waiting_calls = NewWaitingCalls}.

enqueue(Call, WaitingCalls) ->
  {datetime, NotBefore} = Call#queued_call.not_before,
  ordsets:add_element({NotBefore, Call}, WaitingCalls).

dispatch([]) -> [];
dispatch(Queue = [{_, Call} | Rest]) ->
  case Call:should_trigger() of
    true ->
      channel_queue:enqueue(Call),
      dispatch(Rest);
    false ->
      Queue
  end.