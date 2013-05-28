-module(call_log_srv).
-export([new/1, error/3, info/3, trace/3, trace_record/5, update/2, id/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {call_log, owner_pid}).

-include("db.hrl").

new(CallLog) ->
  {ok, Pid} = gen_server:start(?MODULE, {CallLog, self()}, []),
  {?MODULE, Pid}.

error(Message, Details, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {log, error, Message, Details}).

info(Message, Details, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {log, info, Message, Details}).

trace(Message, Details, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {log, trace, Message, Details}).

trace_record(CallFlowId, StepId, StepName, Result, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {trace_record, CallFlowId, StepId, StepName, Result}).

update(Fields, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {update, Fields}).

id({?MODULE, Pid}) ->
  gen_server:call(Pid, get_id).

%% @private
init({CallLog, Owner}) ->
  monitor(process, Owner),
  case CallLog#call_log.id of
    undefined -> gen_server:cast(self(), create);
    _ -> ok
  end,
  {ok, #state{call_log = CallLog, owner_pid = Owner}}.

%% @private
handle_call(get_id, _From, State = #state{call_log = CallLog}) ->
  {reply, CallLog#call_log.id, State}.

%% @private
handle_cast(create, State = #state{call_log = CallLog}) ->
  NewCallLog = CallLog:create(),
  {noreply, State#state{call_log = NewCallLog}};

handle_cast({log, Level, Message, Details}, State = #state{call_log = CallLog}) ->
  CallLog:Level(Message, Details),
  {noreply, State};

handle_cast({update, Fields}, State = #state{call_log = CallLog}) ->
  NewCallLog = CallLog:update(Fields),
  {noreply, State#state{call_log = NewCallLog}};

handle_cast({trace_record, CallFlowId, StepId, StepName, Result}, State = #state{call_log = CallLog}) ->
  TraceRecord = #trace_record{
    call_flow_id = CallFlowId,
    step_id = StepId,
    step_name = StepName,
    call_id = CallLog#call_log.id,
    result = Result
  },
  TraceRecord:save(),
  {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{owner_pid = Pid}) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
