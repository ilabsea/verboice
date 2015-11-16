-module(hangup_and_callback).
-export([run/2]).
-include("db.hrl").
-include("session.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

run(Args, Session) ->
  Prefix = proplists:get_value(dial_prefix, Args),
  When = proplists:get_value('when', Args),
  SelectedCallFlowId = proplists:get_value(selected_call_flow_id, Args),
  Retries = proplists:get_value(retries, Args),

  NotBefore = not_before(When, Args),

  NewSessionId = session:new_id(),
  {ok, NewPid} = session:new(NewSessionId),

  CallFlowId = case SelectedCallFlowId of
    undefined -> Session#session.call_flow#call_flow.id;
    X -> X
  end,

  CallFlow = call_flow:find(CallFlowId),
  Project = project:find(CallFlow#call_flow.project_id),

  CallLog = Session#session.call_log,
  NewCallLog = call_log_srv:new(NewSessionId, #call_log{
    account_id = Session#session.channel#channel.account_id,
    project_id = CallFlow#call_flow.project_id,
    state = "suspended",
    direction = "outgoing",
    channel_id = Session#session.channel#channel.id,
    address = Session#session.address,
    started_at = calendar:universal_time(),
    call_flow_id = CallFlow#call_flow.id,
    store_log_entries = Project#project.store_call_log_entries,
    parent_id = CallLog:id()
  }),

  NotBefore = util:time_from_now(application:get_env(verboice, seconds_for_call_back, 15)),
  QueuedCall = #queued_call{
    not_before = {datetime, NotBefore},
    session_id = NewSessionId,
    channel_id = Session#session.channel#channel.id,
    address = dial_address(Session#session.address, Prefix),
    state = list_to_binary("queued"),
    call_log_id = NewCallLog:id(),
    call_flow_id = CallFlow#call_flow.id,
    project_id = Project#project.id,
    retries = 0
  },

  case SelectedCallFlowId of
    undefined ->
      scheduler:enqueue(QueuedCall),
      {{respawn_continuous, NewPid}, Session};
    _ ->
      RetrySchedule = schedule:create( #schedule{
        name= "callback",
        retries= Retries,
        project_id=Project#project.id,
        weekdays= <<"0,1,2,3,4,5,6">>,
        disabled= 1,
        time_from="00:00",
        time_to="23:59"
      }),

      QueuedCallWithSchedule = QueuedCall#queued_call{ session_id = undefined, schedule_id = RetrySchedule:id() },
      scheduler:enqueue(QueuedCallWithSchedule),
      {{respawn, NewPid}, Session}
  end.

dial_address(Address, undefined) -> Address;
dial_address(Address, []) -> Address;
dial_address(Address, Prefix) ->
  PrefixBin = list_to_binary(Prefix),
  PrefixSize = byte_size(PrefixBin),
  case Address of
    <<PrefixBin:PrefixSize/binary, _/binary>> -> Address;
    _ -> <<PrefixBin/binary, Address/binary>>
  end.

-ifdef(TEST).
dial_address_test() ->
  ?assertEqual(<<"123">>, dial_address(<<"123">>, undefined)),
  ?assertEqual(<<"123">>, dial_address(<<"123">>, [])),
  ?assertEqual(<<"0123">>, dial_address(<<"123">>, "0")),
  ?assertEqual(<<"123">>, dial_address(<<"123">>, "12")).
-endif.

not_before(When, Args) ->
  case When of
    "later" ->
      Delay = proplists:get_value(delay, Args),
      Seconds = util:parse_short_time(Delay),
      datetime_utils:time_from_now(Seconds);
    _ ->
      datetime_utils:time_from_now(15)
  end.
