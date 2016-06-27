-module(session_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

-record(state, {session_id, session, resume_ptr, pbx_pid, flow_pid, hibernated}).

notify_status_on_completed_ok_test() ->
  Session = #session{address = <<"123">>, call_log = {call_log_srv}, status_callback_url = <<"http://foo.com">>},
  meck:new(call_log_srv, [stub_all]),
  meck:expect(call_log_srv, id, 1, 1),
  meck:expect(call_log, find, 1, #call_log{address = "1000", channel_id = 1, started_at = {"_", calendar:universal_time()}}),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123&CallDuration=0", []}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, Session, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().

notify_status_on_completed_sends_call_duration_test() ->
  Session = #session{address = <<"123">>, call_log = {call_log_srv}, status_callback_url = <<"http://foo.com">>, started_at = {{2014,12,11},{16,0,0}}},
  meck:new(calendar, [unstick, passthrough]),
  meck:expect(calendar, universal_time, fun() -> {{2014,12,11},{16,0,5}} end),

  meck:new(call_log_srv, [stub_all]),
  meck:expect(call_log_srv, id, 1, 1),
  meck:expect(call_log, find, 1, #call_log{address = "1000", channel_id = 1, started_at = {"_", calendar:universal_time()}}),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123&CallDuration=5", []}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, Session, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().

notify_status_on_completed_ok_with_callback_params_test() ->
  Session = #session{address = <<"123">>, call_log = {call_log_srv}, status_callback_url = <<"http://foo.com">>, callback_params = [{"foo", "1"}]},
  meck:new(call_log_srv, [stub_all]),
  meck:expect(call_log_srv, id, 1, 1),
  meck:expect(call_log, find, 1, #call_log{address = "1000", channel_id = 1, started_at = {"_", calendar:universal_time()}}),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123&CallDuration=0&foo=1", []}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, Session, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().

notify_status_with_http_credentials_test() ->
  Session = #session{address = <<"123">>, call_log = {call_log_srv}, status_callback_url = <<"http://foo.com">>, status_callback_user = "user", status_callback_password = "pass"},
  meck:new(call_log_srv, [stub_all]),
  meck:expect(call_log_srv, id, 1, 1),
  meck:expect(call_log, find, 1, #call_log{address = "1000", channel_id = 1, started_at = {"_", calendar:universal_time()}}),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123&CallDuration=0", [{"Authorization", "Basic dXNlcjpwYXNz"}]}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, Session, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().
