-module(session).
-export([start_link/1, new/0, new/1, find/1, answer/2, answer/4, dial/4, reject/2, stop/1, resume/1, default_variables/1, create_default_erjs_context/2]).
-export([no_ack/1]).
-export([language/1]).
-export([new_id/0, set_pointer/2]).
-compile([{parse_transform, lager_transform}]).

% FSM Description
% Possible states: ready, dialing, in_progress, completed
% Initial state: ready
%
% ready(dial) -> dialing
% ready(answer) -> in_progress
% dialing(answer) -> in_progress
% dialing(error | no_answer | busy) -> failed
% in_progress(error | hangup) -> failed
% in_progress(done) -> completed
% in_progress(suspend) -> ready

-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([ready/2, ready/3, dialing/2, in_progress/2, in_progress/3, matches/2]).

-define(SESSION(Id), {session, Id}).
-define(TIMEOUT_DIALING, 60 * 1000). %  1 Minute
-define(TIMEOUT_INPROGRESS, 30 * 60 * 1000). % 30 Minutes
-define(TIMEOUT_SESSION, 30 * 60 * 1000). % 30 Minutes

% delay for executing the job to push call results to Fusion Tables
-define(PUSH_DELAY_SECONDS, 60).

-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

-record(state, {session_id, session, resume_ptr, pbx_pid, flow_pid, hibernated}).

start_link(HibernatedSession = #hibernated_session{session_id = SessionId}) ->
  StringSessionId = util:to_string(SessionId),
  gen_fsm:start_link({global, ?SESSION(StringSessionId)}, ?MODULE, HibernatedSession, []);
start_link(SessionId) ->
  StringSessionId = util:to_string(SessionId),
  gen_fsm:start_link({global, ?SESSION(StringSessionId)}, ?MODULE, StringSessionId, []).

new() ->
  new(new_id()).

new(HibernatedSession = #hibernated_session{}) ->
  HibernatedSessionId = HibernatedSession#hibernated_session.session_id,
  SessionSpec = {util:to_string(HibernatedSessionId), {session, start_link, [HibernatedSession]}, temporary, 5000, worker, [session]},
  supervisor:start_child(session_sup, SessionSpec);
new(SessionId) ->
  SessionSpec = {SessionId, {session, start_link, [SessionId]}, temporary, 5000, worker, [session]},
  supervisor:start_child(session_sup, SessionSpec).

new_id() ->
  uuid:to_string(uuid:v4()).

-spec find(binary() | string()) -> undefined | pid().
find(SessionId) ->
  SessionPid = global:whereis_name(?SESSION(util:to_string(SessionId))),
  case SessionPid of
    undefined ->
      HibernatedSession = hibernated_session:find({session_id, SessionId}),
      case HibernatedSession of
        undefined -> undefined;
        _ ->
          {ok, NewSessionPid} = new(HibernatedSession),
          NewSessionPid
      end;
    _ -> SessionPid
  end.

-spec answer(pid(), pbx:pbx(), integer(), binary()) -> any().
answer(SessionPid, Pbx, ChannelId, CallerId) ->
  gen_fsm:send_event(SessionPid, {answer, Pbx, ChannelId, CallerId}).

-spec answer(pid(), pbx:pbx()) -> any().
answer(SessionPid, Pbx) ->
  gen_fsm:send_event(SessionPid, {answer, Pbx}).

dial(SessionPid, RealBroker, Channel, QueuedCall) ->
  gen_fsm:sync_send_event(SessionPid, {dial, RealBroker, Channel, QueuedCall}).

reject(SessionPid, Reason) ->
  gen_fsm:send_event(SessionPid, {reject, Reason}).

stop(SessionPid) ->
  gen_fsm:send_all_state_event(SessionPid, stop).

resume(SessionPid) ->
  gen_fsm:send_event(SessionPid, resume).

no_ack(SessionPid) ->
  gen_fsm:send_event(SessionPid, timeout).

matches(SessionPid, Criteria) ->
  try gen_fsm:sync_send_all_state_event(SessionPid, {matches, Criteria}, 100)
  catch
    exit:_ -> false
  end.

language(#session{js_context = JsContext, default_language = DefaultLanguage}) ->
  case erjs_context:get(var_language, JsContext) of
    undefined -> DefaultLanguage;
    Language -> Language
  end.

set_pointer(SessionPid, Ptr) ->
  gen_fsm:sync_send_event(SessionPid, {set_pointer, Ptr}).

%% @private
init(HibernatedSession = #hibernated_session{ data = #hibernated_session_data{resume_ptr = ResumePtr, poirot_activity = Activity }}) ->
  poirot:set_current(Activity),
  Session = HibernatedSession:wake_up(),
  {ok, ready, #state{session_id = Session#session.session_id, session = Session, resume_ptr = ResumePtr}};

init(SessionId) ->
  poirot:push(poirot:new_activity("Session ~p", [SessionId])),
  {ok, ready, #state{session_id = SessionId}}.

ready({answer, Pbx, ChannelId, CallerId}, State = #state{session_id = SessionId}) ->
  lager:info("Session (~p) answer", [SessionId]),
  monitor(process, Pbx:pid()),

  NewSession = case State#state.session of
    undefined ->
      Channel = channel:find(ChannelId),
      CallFlow = call_flow:find(Channel#channel.call_flow_id),
      Project = project:find(CallFlow#call_flow.project_id),
      Contact = get_contact(CallFlow#call_flow.project_id, CallerId, 1),
      ContactAddress = contact:find_or_create_contact_address(CallerId, Contact),
      CallLog = call_log_srv:new(SessionId, #call_log{
        account_id = Channel#channel.account_id,
        project_id = CallFlow#call_flow.project_id,
        state = "active",
        direction = "incoming",
        channel_id = ChannelId,
        address = ContactAddress#contact_address.address,
        started_at = calendar:universal_time(),
        call_flow_id = CallFlow#call_flow.id,
        store_log_entries = Project#project.store_call_log_entries
      }),
      Flow = call_flow:flow(CallFlow),
      {StatusUrl, StatusUser, StatusPass} = project:status_callback(Project),

      #session{
        session_id = SessionId,
        pbx = Pbx,
        channel = Channel,
        flow = Flow,
        call_flow = CallFlow,
        call_log = CallLog,
        project = Project,
        address = CallerId,
        contact = Contact,
        status_callback_url = StatusUrl,
        status_callback_user = StatusUser,
        status_callback_password = StatusPass,
        started_at = {datetime, calendar:universal_time()}
      };
    Session -> Session#session{pbx = Pbx}
  end,

  poirot:add_meta([
    {address, CallerId},
    {project_id, NewSession#session.project#project.id},
    {call_log_id, (NewSession#session.call_log):id()},
    {channel_id, ChannelId},
    {channel_name, NewSession#session.channel#channel.name}
  ]),

  notify_status('in-progress', NewSession),
  FlowPid = spawn_run(NewSession, State#state.resume_ptr),

  {next_state, in_progress, State#state{pbx_pid = Pbx:pid(), flow_pid = FlowPid, session = NewSession}, ?TIMEOUT_INPROGRESS}.

ready({set_pointer, Ptr}, _From, State) ->
  {reply, ok, ready, State#state{resume_ptr = Ptr}};

ready({dial, _, _, QueuedCall = #queued_call{address = undefined}}, _From, State) ->
  lager:error("Refusing to make a call to an undefined address (queued call id ~p)", [QueuedCall#queued_call.id]),
  CallLog = call_log:find(QueuedCall#queued_call.call_log_id),
  CallLog:update([{state, "failed"}, {fail_reason, "invalid address"}, {finished_at, calendar:universal_time()}]),
  {stop, normal, error, State};

ready({dial, RealBroker, Channel, QueuedCall}, _From, State = #state{session_id = SessionId, resume_ptr = ResumePtr}) ->
  lager:info("Session (~p) dial", [SessionId]),

  AddressWithoutVoipPrefix = channel:address_without_voip_prefix(Channel, QueuedCall#queued_call.address),

  AddressWithoutVoipPrefix = channel:address_without_voip_prefix(Channel, QueuedCall#queued_call.address),

  NewSession = case State#state.session of
    undefined ->
      CallLog = call_log_srv:new(SessionId, call_log:find(QueuedCall#queued_call.call_log_id)),
      Contact = get_contact(QueuedCall#queued_call.project_id, AddressWithoutVoipPrefix, QueuedCall#queued_call.call_log_id),
      Session = QueuedCall:start_session(),

      poirot:add_meta([
        {address, QueuedCall#queued_call.address},
        {project_id, QueuedCall#queued_call.project_id},
        {call_log_id, QueuedCall#queued_call.call_log_id},
        {channel_id, Channel#channel.id},
        {channel_name, Channel#channel.name}
      ]),

      Session#session{
        session_id = SessionId,
        channel = Channel,
        call_log = CallLog,
        contact = Contact,
        started_at = {datetime, calendar:universal_time()}
      };

    Session ->
      CallLog = Session#session.call_log,
      Session#session{queued_call = QueuedCall, address = AddressWithoutVoipPrefix}
  end,

  % Don't the started_at if we are resuming an existing session
  case ResumePtr of
    undefined ->
      CallLog:update({started_at, calendar:universal_time()});
    _ -> ok
  end,
  
  case channel:is_approved(Channel) of
    true ->
      case channel:enabled(Channel) of
        true ->
          QuotaReaches = case channel_quota:find([{channel_id, Channel#channel.id}]) of
            undefined -> false;
            ChannelQuota -> ChannelQuota:enabled() andalso ChannelQuota:blocked()
          end,
          case QuotaReaches of
            true ->
              {_, _, NewSession2} = finalize({failed, blocked}, State#state{session = NewSession}),
              {stop, normal, error, State#state{session = NewSession2}};
            _ -> 
              case RealBroker:dispatch(NewSession) of
                {error, unavailable} ->
                  {stop, normal, unavailable, State#state{session = NewSession}};
                {error, Reason} ->
                  {_, _, NewSession2} = finalize({failed, Reason}, State#state{session = NewSession}),
                  {stop, normal, error, State#state{session = NewSession2}};
                _ ->
              lager:info("Dialing to ~s through channel ~s", [QueuedCall#queued_call.address, Channel#channel.name]),
                  notify_status(ringing, NewSession),
                  CallLog:update([{state, "active"}, {fail_reason, undefined}]),
                  {reply, ok, dialing, State#state{session = NewSession}, ?TIMEOUT_DIALING}
              end
          end;
        _ -> 
          {_, _, NewSession2} = finalize({failed, disabled}, State#state{session = NewSession}),
          {stop, normal, error, State#state{session = NewSession2}}
      end;
    _ ->
      {_, _, NewSession2} = finalize({failed, disabled}, State#state{session = NewSession}),
      {stop, normal, error, State#state{session = NewSession2}}
    end.

dialing({answer, Pbx}, State = #state{session_id = SessionId, session = Session = #session{call_log = CallLog}, resume_ptr = Ptr}) ->
  lager:info("Session (~p) answer", [SessionId]),
  NewQueuedCall = Session#session.queued_call#queued_call{answered_at = {datetime, calendar:universal_time()}},
  NewQueuedCallSession = Session#session{queued_call = NewQueuedCall},

  CallLog:update([{started_at, calendar:universal_time()}]),

  monitor(process, Pbx:pid()),
  NewSession = NewQueuedCallSession#session{pbx = Pbx, started_at = calendar:universal_time() },
  notify_status('in-progress', NewSession),
  FlowPid = case Ptr of
    undefined -> spawn_run(NewSession, Ptr);
    _ ->
      RusumeSession = store_default_language(NewSession),
      spawn_run(RusumeSession, Ptr)
  end,

  {next_state, in_progress, State#state{pbx_pid = Pbx:pid(), flow_pid = FlowPid, session = NewSession}, ?TIMEOUT_INPROGRESS};

dialing({reject, Reason}, State = #state{session = Session = #session{session_id = SessionId, call_log = CallLog}}) ->
  lager:info("Session (~p) rejected, reason: ~p", [SessionId, Reason]),
  CallLog:error(["Call was rejected. (Reason: ", atom_to_list(Reason),")"], []),
  Status = case Reason of
    busy -> busy;
    no_answer -> 'no-answer';
    _ -> failed
  end,
  notify_status(Status, Session),
  finalize({failed, Reason}, State);

dialing(timeout, State = #state{session = Session}) ->
  IsTimeout = is_timeout(Session, ?TIMEOUT_DIALING),

  if
    IsTimeout ->
      timeout(Session),
      notify_status(busy, Session),
      finalize({failed, timeout}, State);
    true -> 
      {next_state, dialing, State, ?TIMEOUT_DIALING}
  end.

in_progress({completed, ok}, State = #state{session = Session}) ->
  notify_status(completed, Session),
  finalize(completed, State);

in_progress({completed, {failed, Reason}}, State = #state{session = Session}) ->
  notify_status(failed, Session),
  finalize({failed, Reason}, State);

in_progress(timeout, State = #state{session = Session = #session{pbx = Pbx}}) ->
  IsTimeout = is_timeout(Session, ?TIMEOUT_SESSION),

  if
    IsTimeout -> 
      timeout(Session),
      try
        notify_status(no_ack, Session),
        finalize({failed, no_ack}, State)
      after
        Pbx:hangup()
      end;
    true -> {next_state, in_progress, State}
  end.

in_progress({suspend, NewSession, Ptr}, _From, State = #state{session = Session = #session{session_id = SessionId}}) ->
  lager:info("Session (~p) suspended", [SessionId]),
  channel_queue:unmonitor_session(Session#session.channel#channel.id, self()),
  {reply, ok, ready, State#state{pbx_pid = undefined, flow_pid = undefined, resume_ptr = Ptr, session = NewSession}};
  
in_progress({hibernate, NewSession, Ptr}, _From, State = #state{session = _Session = #session{session_id = SessionId}}) ->
  lager:info("Session (~p) hibernated", [SessionId]),
  Data = #hibernated_session_data{
    flow = NewSession#session.flow,
    stack = NewSession#session.stack,
    js_context = NewSession#session.js_context,
    channel_id = NewSession#session.channel#channel.id,
    call_flow = NewSession#session.call_flow,
    call_log_id = (NewSession#session.call_log):id(),
    project_id = NewSession#session.project#project.id,
    address = NewSession#session.address,
    contact_id = NewSession#session.contact#contact.id,
    default_language = NewSession#session.default_language,
    status_callback_url = NewSession#session.status_callback_url,
    status_callback_user = NewSession#session.status_callback_user,
    status_callback_password = NewSession#session.status_callback_password,
    resume_ptr = Ptr,
    poirot_activity = poirot:current()
  },
  HibernatedSession = #hibernated_session{session_id = SessionId, data = Data},
  HibernatedSession:create(),
  {stop, normal, ok, State#state{hibernated = true}};

in_progress({respawn, NewSessionPid, Ptr}, _From, State = #state{session = Session}) ->
  session:set_pointer(NewSessionPid, Ptr),
  
  channel_queue:unmonitor_session(Session#session.channel#channel.id, self()),
  
  notify_status(completed, Session),
  finalize(completed, State).

notify_status(Status, Session = #session{call_log = CallLog, address = Address, callback_params = CallbackParams, started_at = StartedAt}) ->
  case Session#session.status_callback_url of
    undefined -> ok;
    <<>> -> ok;
    Url ->
      CallSid = util:to_string(CallLog:id()),
      spawn(fun() ->
        Uri = uri:parse(binary_to_list(Url)),
        Duration = case StartedAt of
          undefined -> 0;
          {_, StartedAtValue} ->
            Now = calendar:universal_time(),
            calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(StartedAtValue)
        end,
        QueryString = [{"CallSid", CallSid}, {"CallStatus", Status}, {"From", Address}, {"CallDuration", erlang:integer_to_list(Duration)} | CallbackParams],
        AuthOptions = case Session#session.status_callback_user of
          undefined -> [];
          [] -> [];
          <<>> -> [];
          User -> [{basic_auth, {User, Session#session.status_callback_password}}]
        end,
        (Uri#uri{query_string = QueryString}):get([{full_result, false} | AuthOptions])
      end)
  end.

handle_event(stop, _, State) ->
  {stop, normal, State}.

handle_sync_event({matches, Criteria}, _From, StateName, State = #state{session = Session}) ->
  MatchResult = case Session of
    undefined -> false;
    _ ->
     case Criteria of
      {project, ProjectId} ->
        Session#session.project#project.id == ProjectId;
      {channel, ChannelId} ->
        Session#session.channel#channel.id == ChannelId;
      {call_flow, CallFlowId} ->
        Session#session.call_flow#call_flow.id == CallFlowId;
      _ -> false
    end
  end,
  {reply, MatchResult, StateName, State, ?TIMEOUT_INPROGRESS};

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, Reason}, _, State = #state{session = Session, pbx_pid = Pid}) ->
  notify_status(failed, Session),
  lager:error("PBX closed unexpectedly with reason: ~s", [Reason]),
  finalize({failed, {error, Reason}}, State);

handle_info({'DOWN', _Ref, process, Pid, Reason}, _, State = #state{session = Session, flow_pid = Pid}) ->
  Pbx = Session#session.pbx,
  Pbx:terminate(),
  notify_status(failed, Session),
  lager:error("Flow process died unexpectedly with reason: ~s", [Reason]),
  finalize({failed, {error, Reason}}, State);

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State, ?TIMEOUT_INPROGRESS}.

%% @private
terminate(_Reason, _, #state{hibernated = true}) ->
  ok;
terminate(Reason, _, #state{session_id = SessionId, session = Session}) ->
  push_results(Session),
  case Reason of
    normal -> lager:info("Session (~p) terminated normally", [SessionId]);
    _ -> lager:warning("Session (~p) terminated with reason: ~p", [SessionId, Reason])
  end,
  poirot:pop().

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

push_results(#session{call_flow = #call_flow{id = CallFlowId, store_in_fusion_tables = 1}, call_log = CallLog}) ->
  Task = ["--- !ruby/struct:CallFlow::FusionTablesPush::Pusher\ncall_flow_id: ", integer_to_list(CallFlowId),
    "\ncall_log_id: ", integer_to_list(CallLog:id()), "\n"],
  delayed_job:enqueue(Task, ?PUSH_DELAY_SECONDS);
push_results(_) -> ok.

finalize(completed, State = #state{session = Session =  #session{call_log = CallLog}}) ->
  Retries = case Session#session.queued_call of
    undefined -> 0;
    QueuedCall -> QueuedCall#queued_call.retries
  end,

  Call = call_log:find(CallLog:id()),
  CallLog:completed(total_call_duraction(Call, Session), Retries),

  {stop, normal, State};

finalize({failed, Reason}, State = #state{session = Session = #session{call_log = CallLog}}) ->
  StopReason = case Reason of
    {error, Error} -> Error;
    _ ->
      {Retries, NewState} = case Session#session.queued_call of
        undefined -> {0, "failed"};
        QueuedCall ->
          % reset answered_at for reschedule
          NewQueuedCall = QueuedCall#queued_call{answered_at = undefined},
          NewRetries = case NewQueuedCall#queued_call.retries of
            undefined -> 0;
            Value -> Value
          end,

          case should_reschedule(Reason) of
            true -> 
              case NewQueuedCall:reschedule() of
                no_schedule -> {NewRetries, failed};
                max_retries ->
                  CallLog:error("Max retries exceeded", []),
                  {NewRetries, "failed"};
                #queued_call{not_before = {datetime, NotBefore}} ->
                  CallLog:info(["Call rescheduled to start at ", httpd_util:rfc1123_date(calendar:universal_time_to_local_time(NotBefore))], []),
                  {NewRetries, "queued"}
              end;
            false -> {NewRetries, "failed"}
          end
      end,
    
      % end step interaction
      if
        Reason =:= hangup; Reason =:= error -> CallLog:end_step_interaction();
        true -> ok
      end,

      Call = call_log:find(CallLog:id()),
      Duration = total_call_duraction(Call,Session),

      if
        NewState == failed; NewState == "failed" -> 
          CallLog:update([{state, NewState}, {fail_reason, io_lib:format("~p", [Reason])}, {finished_at, calendar:universal_time()}, {retries, Retries}, {duration, Duration}]);
        true -> 
          CallLog:update([{state, NewState}, {fail_reason, io_lib:format("~p", [Reason])}, {retries, Retries}, {duration, Duration}])
      end,
      normal
  end,

  {stop, StopReason, State}.

spawn_run(Session, undefined) ->
  RunSession = store_default_language(Session),
  spawn_run(RunSession, 1);

spawn_run(Session = #session{pbx = Pbx}, Ptr) ->
  SessionPid = self(),
  SessionActivity = poirot:current(),
  spawn_monitor(fun() ->
    poirot:new_inside(SessionActivity, "Session worker process", async, fun() ->
      lager:info("Start"),
      try run(Session, Ptr) of
        {suspend, NewSession, NewPtr} ->
          close_user_step_activity(NewSession),
          gen_fsm:sync_send_event(SessionPid, {suspend, NewSession#session{in_user_step_activity = false}, NewPtr});
        {hibernate, NewSession, NewPtr} ->
          close_user_step_activity(NewSession),
          gen_fsm:sync_send_event(SessionPid, {hibernate, NewSession#session{in_user_step_activity = false}, NewPtr});
        {respawn, NewSessionPid, NewPtr} ->
          gen_fsm:sync_send_event(SessionPid, {respawn, NewSessionPid, NewPtr});
        {Result, NewSession = #session{js_context = JsContext}} ->
          close_user_step_activity(NewSession),
          Status = erjs_context:get(status, JsContext),
          FlowResult = flow_result(Result, Status),
          case FlowResult of
            {failed, "marked as failed"} ->
              notify_status(marked_as_failed, NewSession),
              finalize({failed, marked_as_failed}, #state{session = NewSession});
            _ -> gen_fsm:send_event(SessionPid, {completed, FlowResult})
          end
      after
        catch Pbx:terminate()
      end
    end)
  end).

close_user_step_activity(#session{in_user_step_activity = true}) -> poirot:pop();
close_user_step_activity(_) -> ok.

flow_result(ok, "failed") -> {failed, "marked as failed"};
flow_result({failed, _}, "successful") -> ok;
flow_result(Result, _) -> Result.

get_contact(ProjectId, undefined, CallLogId) ->
  Address = "Anonymous" ++ integer_to_list(CallLogId),
  contact:create_anonymous(ProjectId, Address);
get_contact(ProjectId, Address, _) ->
  contact:find_or_create_with_address(ProjectId, Address).

default_variables(#session{address = Address, contact = Contact, queued_call = QueuedCall, project = #project{id = ProjectId, time_zone = TimeZone}, call_log = CallLog, started_at = StartedAt}) ->
  CallLogId = util:to_string(CallLog:id()),
  Context = create_default_erjs_context(CallLogId, Address),
  ProjectVars = project_variable:names_for_project(ProjectId),
  Variables = persisted_variable:find_all({contact_id, Contact#contact.id}),
  JsContext = erjs_context:set(var_caller_id, binary_to_list(Address), Context),
  NewJsContext = erjs_context:set(var_call_at, datetime_utils:strftime(datetime_utils:in_zone(TimeZone, StartedAt)), JsContext),
  DefaultContext = default_variables(NewJsContext, ProjectVars, Variables),
  initialize_context(DefaultContext, QueuedCall).

create_default_erjs_context(CallLogId, PhoneNumber) ->
  erjs_context:new([
    {record_url, fun(Key) ->
      {ok, BaseUrl} = application:get_env(base_url),
      BaseUrl ++ "/calls/" ++ CallLogId ++ "/results/" ++ util:to_string(Key)
    end},
    {'_get_var', fun(Name, Context) ->
      Value = erjs_context:get(Name, Context),
      case Value of
        undefined ->
          VarName = binary_to_atom(iolist_to_binary(["var_", atom_to_list(Name)]), utf8),
          erjs_context:get(VarName, Context);
        _ -> Value
      end
    end},
    {'split_digits', fun(Value) ->
      Result = re:replace(Value,"\\d"," &",[{return,list}, global]),
      io:format("result: ~p~n", [Result]),
      Result
    end},
    {phone_number, util:to_string(PhoneNumber)}
  ]).

initialize_context(Context, QueuedCall = #queued_call{}) ->
  lists:foldl(fun({Name, Value}, C) ->
    case Value of
      undefined -> C;
      [] -> C;
      <<>> -> C;
      _ ->
        VarName = binary_to_atom(iolist_to_binary(["var_", Name]), utf8),
        erjs_context:set(VarName, Value, C)
    end
  end, Context, QueuedCall:variables());
initialize_context(Context, _) -> Context.

default_variables(Context, _ProjectVars, []) -> Context;
default_variables(Context, ProjectVars, [#persisted_variable{value = undefined} | Rest]) ->
  default_variables(Context, ProjectVars, Rest);
default_variables(Context, ProjectVars, [Var | Rest]) ->
  VarName = case Var#persisted_variable.implicit_key of
    <<"language">> -> var_language;
    <<"sms_number">> -> var_sms_number;
    undefined -> proplists:get_value(Var#persisted_variable.project_variable_id, ProjectVars)
  end,
  VarValue = binary_to_list(Var#persisted_variable.value),
  default_variables(erjs_context:set(VarName, VarValue, Context), ProjectVars, Rest).

run(Session = #session{flow = Flow}, Ptr) when Ptr > length(Flow) -> end_flow(Session);
run(Session = #session{flow = Flow, stack = Stack}, Ptr) ->
  Command = lists:nth(Ptr, Flow),
  try eval(Command, Session) of
    {Action, NewSession} ->
      case Action of
        next ->
          run(NewSession, Ptr + 1);
        {goto, N} ->
          run(NewSession, N + 1);
        {exec, NewFlow} ->
          case has_ended(Flow, Ptr + 1) of
            true -> run(NewSession#session{flow = NewFlow}, 1);
            false -> run(NewSession#session{flow = NewFlow, stack = [{Flow, Ptr + 1} | Stack]}, 1)
          end;
        finish ->
          end_flow(NewSession);
        suspend ->
          {suspend, NewSession, Ptr + 1};
        hibernate ->
          {hibernate, NewSession, Ptr + 1};
        {respawn, NewSessionPid} ->
          {respawn, NewSessionPid, undefined};
        {respawn_continuous, NewSessionPid} ->
          {respawn, NewSessionPid, Ptr + 1}
      end
  catch
    hangup ->
      lager:warning("The user hang up"),
      poirot:add_meta([{error, <<"The user hang up">>}]),
      {{failed, hangup}, Session};
    {hangup, NewSession} ->
      lager:warning("The user hang up"),
      poirot:add_meta([{error, <<"The user hang up">>}]),
      {{failed, hangup}, NewSession};
    Reason when is_list(Reason) ->
      poirot:add_meta([{error, iolist_to_binary(io_lib:format("~s", [Reason]))}]),
      lager:error("~s", [Reason]),
      {{failed, Reason}, Session};
    Reason ->
      poirot:add_meta([{error, iolist_to_binary(io_lib:format("~p", [Reason]))}]),
      lager:error("~p", [Reason]),
      {{failed, Reason}, Session};
    Class:Error ->
      poirot:add_meta([{error, iolist_to_binary(io_lib:format("Fatal Error: ~p", [Error]))}]),
      lager:error("Error during session ~p: ~p:~p~n~p~n",
        [Session#session.session_id, Class, Error, erlang:get_stacktrace()]),
      {{failed, {error, Error}}, Session}
  end.

end_flow(Session = #session{stack = []}) -> {ok, Session};
end_flow(Session = #session{stack = [{Flow, Ptr} | Rest]}) ->
  run(Session#session{flow = Flow, stack = Rest}, Ptr).

has_ended(Flow, Ptr) when Ptr > length(Flow) ->  true;
has_ended(Flow, Ptr) -> lists:nth(Ptr, Flow) =:= stop.

eval(stop, Session) -> {finish, Session};
eval([Command, Args], Session) -> Command:run(Args, Session);
eval(Command, Session) -> Command:run(Session).

total_call_duraction(Call, Session) ->
   Call:duration() + answer_duration(Session).

answer_duration(#session{call_log = CallLog, queued_call = QueuedCall}) ->
  AnsweredAt = case QueuedCall of
    undefined ->
      Call = call_log:find(CallLog:id()),
      {_, StartedAt} = Call#call_log.started_at,
      StartedAt;
    _ ->
      % reject and no_answer has no answered_at
      case QueuedCall#queued_call.answered_at of
        undefined -> undefined;
        {_, NewAnsweredAt} -> NewAnsweredAt
      end
  end,

  Now = calendar:universal_time(),
  case AnsweredAt of
    undefined -> 0;
    _ -> calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(AnsweredAt)
  end.

should_reschedule(marked_as_failed) -> false;
should_reschedule(hangup) -> false;
should_reschedule(_) -> true.

store_default_language(Session = #session{project = Project}) ->
  JsContext = default_variables(Session),
  Session#session{js_context = JsContext, default_language = project:default_language(Project)}.

%% @private
is_timeout(#session{started_at = StartedAt}, TimeoutIn) ->
  Now = calendar:universal_time(),
  {_, S} = StartedAt,

  Diff = 1000 * (calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(S)),
  Diff >= TimeoutIn.

%% @private
timeout(Session = #session{session_id = SessionId, channel = #channel{id = ChannelId}}) ->
  error_logger:info_msg("Session (~p) timeout, reason: NOACK", [SessionId]),
  channel_queue:unmonitor_session(ChannelId, self()),
  admin:notify_session_timeout(Session).
