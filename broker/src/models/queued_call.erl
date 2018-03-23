-module(queued_call).
-compile([{parse_transform, lager_transform}]).
-export([reschedule/1, start_session/1, should_trigger/1]).
-export([called_at/1, variables/1, retries/1]).
-define(TABLE_NAME, "queued_calls").
-include("session.hrl").

-define(MAP, [
  {callback_params, yaml_serializer},
  {variables, yaml_serializer}
]).

-include_lib("erl_dbmodel/include/model.hrl").

reschedule(#queued_call{schedule_id = undefined}) -> no_schedule;
reschedule(QueuedCall = #queued_call{schedule_id = ScheduleId}) ->
  try
    Schedule = schedule:find(ScheduleId),
    reschedule(QueuedCall, Schedule)
  catch
    error:Error -> {error, Error}
  end.

reschedule(_, #schedule{retries = undefined}) -> max_retries;
reschedule(Q, S) when Q#queued_call.retries >= length(S#schedule.retries) -> max_retries;
reschedule(Q = #queued_call{retries = Retries, time_zone = TimeZone}, S) ->
  NextRetryOffset = trunc(lists:nth(Retries + 1, S#schedule.retries)),
  TimeZoneOffset = case TimeZone of
    undefined -> 0;
    _ -> tz_server:get_timezone_offset(TimeZone)
  end,
  NextRetry = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + NextRetryOffset + TimeZoneOffset,
  RetryTime = calendar:gregorian_seconds_to_datetime(S:next_available_time(NextRetry) - TimeZoneOffset),
  case should_skip(Q, RetryTime) of
    false ->
      QueuedCall = queued_call:create(Q#queued_call{not_before = {datetime, RetryTime}, retries = Retries + 1}),
      scheduler:enqueue(QueuedCall),
      QueuedCall;
    true ->
      overdue
  end.

start_session(QueuedCall = #queued_call{call_flow_id = CallFlowId}) when is_number(CallFlowId) ->
  CallFlow = call_flow:find(CallFlowId),
  start_session(#session{flow = call_flow:flow(CallFlow), call_flow = CallFlow}, QueuedCall);
start_session(QueuedCall = #queued_call{callback_url = CallbackUrl}) when is_binary(CallbackUrl) ->
  start_session(#session{flow = flow:callback_flow(CallbackUrl)}, QueuedCall);
start_session(QueuedCall = #queued_call{flow = Flow}) ->
  ParsedFlow = try
                 twiml:parse(Flow)
               catch
                 %% If we fail to parse the flow, simply return undefined;
                 %% the session will finalize with status of failed and an
                 %% appropriate error message.
                 _:_ ->
                   lager:warning("Failed to parse TwiML flow: ~p", [Flow]),
                   undefined
               end,
  start_session(#session{flow = ParsedFlow}, QueuedCall).

start_session(Session, QueuedCall) ->
  Project = project:find(QueuedCall#queued_call.project_id),
  {StatusUrl, StatusUser, StatusPass, StatusIncludeVars} = case QueuedCall#queued_call.status_callback_url of
    undefined -> project:status_callback(Project);
    <<>> -> project:status_callback(Project);
    Url -> {Url, undefined, undefined, false}
  end,
  Session#session{
    address = QueuedCall#queued_call.address,
    status_callback_url = StatusUrl,
    status_callback_user = StatusUser,
    status_callback_password = StatusPass,
    status_callback_include_vars = StatusIncludeVars,
    callback_params = QueuedCall#queued_call.callback_params,
    queued_call = QueuedCall,
    project = Project
  }.

should_skip(#queued_call{not_after = undefined}, _) -> false;
should_skip(#queued_call{not_after = {datetime, NotAfter}}, RetryTime) ->
  NotAfter =< RetryTime;
should_skip(_, _) -> false.
should_trigger(#queued_call{not_before = undefined, state = <<"queued">>}) -> true;
should_trigger(#queued_call{not_before = {datetime, NotBefore}, state = <<"queued">>}) ->
  NotBefore =< calendar:universal_time();
should_trigger(_) -> false.

called_at(#queued_call{not_before = undefined, created_at = CreatedAt}) -> CreatedAt;
called_at(#queued_call{not_before = NotBefore}) -> NotBefore.

variables(#queued_call{variables = Vars}) when is_list(Vars) -> Vars;
variables(#queued_call{variables = Vars}) -> yaml_serializer:load(Vars).

retries(#queued_call{retries = undefined}) -> 0;
retries(#queued_call{retries = Retries}) -> Retries.
