-module(call_log).
-export([error/3, info/3, trace/3]).
-export([duration/1, started_at_or_created_at/1, append_step_interaction/2, address_without_prefix/1]).
-define(TABLE_NAME, "call_logs").
-include_lib("erl_dbmodel/include/model.hrl").

error(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("error", CallId, Message, Details).

info(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("info", CallId, Message, Details).

trace(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("trace", CallId, Message, Details).

duration(#call_log{started_at = StartedAt, finished_at = FinishedAt, duration = Duration}) ->
  case Duration of
    undefined ->
      if
        FinishedAt /= undefined, StartedAt /= undefined ->
          {_, NewStartedAt} = StartedAt,
          {_, NewFinishedAt} = FinishedAt,
          calendar:datetime_to_grogerian_seconds(NewFinishedAt) - calendar:datetime_to_gregorian_seconds(NewStartedAt);
        true -> 0
      end;
    NewDuration -> NewDuration
  end.

started_at_or_created_at(#call_log{created_at = {_, CreatedAt}, started_at = StartedAt}) ->
  case StartedAt of
    undefined -> CreatedAt;
    {datetime, Value} -> Value;
    _ -> StartedAt
  end.

append_step_interaction(StepName, CallLog = #call_log{step_interaction = StepInteraction}) ->
  Time = util:time_difference_in_seconds(CallLog:started_at_or_created_at(), calendar:universal_time()),
  Interaction = [StepName, ":", util:to_string(Time)],
  NewStepInteraction = case StepInteraction of
    undefined -> Interaction;
    _ ->
      [util:to_string(StepInteraction), ";", Interaction]
  end,
  CallLog#call_log{step_interaction = binary_util:to_binary(NewStepInteraction)}.

address_without_prefix(#call_log{prefix_called_number = VoipPrefix, address = Address}) ->
  case VoipPrefix of
    undefined -> Address;
    <<>> -> Address;
    _ -> binary:replace(Address, VoipPrefix, <<>>, [global])
  end.
