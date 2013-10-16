-module(functional_test).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun test_app:start/0, fun test_app:stop/1, F}).
-define(test(F), fun() -> test_app:run_test_in_transaction(F) end).
-include("db.hrl").

session_test_() ->
  ?setup([
    ?test(fun run_simple_flow/0),
    ?test(fun run_queued_call/0),
    ?test(fun play_resource_with_default_language/0)
  ]).

run_simple_flow() ->
  Flow = call_flow:make([{broker_flow, flow:serialize([answer, hangup])}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  {ok, SessionPid} = session:new(),
  Pbx = pbx_mock:new([
    {answer, [], ok},
    {hangup, [], ok}
  ]),

  session:answer(SessionPid, Pbx, Channel:id(), <<"1234">>),

  ?assertEqual(normal, integration_test:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).

run_queued_call() ->
  mock_broker:start(),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, flow:serialize([answer, hangup])}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  QueuedCall = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}]),
  scheduler:load(),

  SessionPid = mock_broker:wait_dispatch(QueuedCall#queued_call.id),

  Pbx = pbx_mock:new([
    {answer, [], ok},
    {hangup, [], ok}
  ]),
  session:answer(SessionPid, Pbx),

  ?assertEqual(normal, integration_test:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()),

  meck:unload().

play_resource_with_default_language() ->
  Project = project:make(),
  Resource = resource:make([{project_id, Project}]),
  localized_resource:make(text, [{resource_id, Resource}, {text, "hello"}]),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, flow:serialize([answer, [play_resource, [{resource_guid, Resource#resource.guid}]]])}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  {ok, SessionPid} = session:new(),
  Pbx = pbx_mock:new([
    {answer, [], ok},
    {can_play, [{text, "en"}], true},
    {play, [{text, "en", <<"hello">>}], ok}
  ]),

  session:answer(SessionPid, Pbx, Channel:id(), <<"1234">>),

  ?assertEqual(normal, integration_test:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).
