-module(hangup_and_callback).
-export([run/2]).
-include("db.hrl").
-include("session.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

run(Args, Session) ->
  Prefix = proplists:get_value(dial_prefix, Args),
  SelectedCallFlowId = proplists:get_value(selected_call_flow_id, Args),
  Retries = proplists:get_value(retries, Args),
  NotBefore = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + 15),

  NewSessionId = session:generate_id(),
  {ok, Pid} = session:new(NewSessionId),

  Channel = channel:find(Session#session.channel#channel.id),

  CallFlowId = case SelectedCallFlowId of
    undefined -> Channel#channel.call_flow_id;
    X -> X
  end,

  CallFlow = call_flow:find(CallFlowId),
  Project = project:find(CallFlow#call_flow.project_id),

  CallLog = call_log_srv:new(NewSessionId, #call_log{
    account_id = Channel#channel.account_id,
    project_id = CallFlow#call_flow.project_id,
    state = "suspended",
    direction = "outgoing",
    channel_id = Channel#channel.id,
    address = Session#session.address,
    started_at = calendar:universal_time(),
    call_flow_id = CallFlow#call_flow.id,
    store_log_entries = Project#project.store_call_log_entries
  }),

  QueuedCall  = #queued_call{
                      not_before = {datetime, NotBefore},
                      session_id = NewSessionId,
                      channel_id = Session#session.channel#channel.id,
                      address = dial_address(Session#session.address, Prefix),
                      state = list_to_binary("queued"),
                      call_log_id = CallLog:id(),
                      call_flow_id = CallFlow#call_flow.id,
                      project_id = Project#project.id
                    },

  case SelectedCallFlowId of
    undefined ->
      scheduler:enqueue(QueuedCall),
      {{suspend, Pid}, Session};
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

      QueuedCallWithSchedule = QueuedCall#queued_call{ schedule_id = RetrySchedule:id() },
      scheduler:enqueue(QueuedCallWithSchedule),
      {{suspend_reset_ptr, Pid}, Session}
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
