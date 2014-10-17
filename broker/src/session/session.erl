-module(session).
-export([start_link/1, new/0, new/1, find/1, answer/2, answer/4, dial/4, reject/2, stop/1, resume/1, default_variables/1]).
-export([language/1]).
-export([generate_id/0, set_pointer/2]).

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

-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

-record(state, {session_id, session, resume_ptr, pbx_pid, flow_pid}).

start_link(SessionId) ->
  gen_fsm:start_link({global, ?SESSION(SessionId)}, ?MODULE, SessionId, []).

new() ->
  SessionId = generate_id(),
  new(SessionId).

new(SessionId) ->
  SessionSpec = {SessionId, {session, start_link, [SessionId]}, temporary, 5000, worker, [session]},
  supervisor:start_child(session_sup, SessionSpec).

generate_id() ->
  uuid:to_string(uuid:v4()).

-spec find(string()) -> undefined | pid().
find(SessionId) ->
  global:whereis_name(?SESSION(SessionId)).

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

init(SessionId) ->
  {ok, ready, #state{session_id = SessionId}}.

ready({answer, Pbx, ChannelId, CallerId}, State = #state{session_id = SessionId}) ->
  error_logger:info_msg("Session (~p) answer", [SessionId]),
  monitor(process, Pbx:pid()),

  NewSession = case State#state.session of
    undefined ->
      Channel = channel:find(ChannelId),
      CallFlow = call_flow:find(Channel#channel.call_flow_id),
      Project = project:find(CallFlow#call_flow.project_id),
      CallLog = call_log_srv:new(SessionId, #call_log{
        account_id = Channel#channel.account_id,
        project_id = CallFlow#call_flow.project_id,
        state = "active",
        direction = "incoming",
        channel_id = ChannelId,
        address = CallerId,
        started_at = calendar:universal_time(),
        call_flow_id = CallFlow#call_flow.id,
        store_log_entries = Project#project.store_call_log_entries
      }),
      Contact = get_contact(CallFlow#call_flow.project_id, CallerId, 1),
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
        status_callback_password = StatusPass
      };
    Session -> Session#session{pbx = Pbx}
  end,
  notify_status('in-progress', NewSession),
  FlowPid = spawn_run(NewSession, State#state.resume_ptr),

  {next_state, in_progress, State#state{pbx_pid = Pbx:pid(), flow_pid = FlowPid, session = NewSession}}.

ready({set_pointer, Ptr}, _From, State) ->
  {reply, ok, ready, State#state{resume_ptr = Ptr}};

ready({dial, RealBroker, Channel, QueuedCall}, _From, State = #state{session_id = SessionId}) ->
  error_logger:info_msg("Session (~p) dial", [SessionId]),

  NewSession = case State#state.session of
    undefined ->
      CallLog = call_log_srv:new(SessionId, call_log:find(QueuedCall#queued_call.call_log_id)),
      Contact = get_contact(QueuedCall#queued_call.project_id, QueuedCall#queued_call.address, QueuedCall#queued_call.call_log_id),
      Session = QueuedCall:start_session(),

      Session#session{
        session_id = SessionId,
        channel = Channel,
        call_log = CallLog,
        contact = Contact
      };
    Session ->
      CallLog = Session#session.call_log,
      Session#session{queued_call = QueuedCall, address = QueuedCall#queued_call.address}
  end,

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
              CallLog:info(["Dialing to ", QueuedCall#queued_call.address, " through channel ", Channel#channel.name], []),
              notify_status(ringing, NewSession),
              CallLog:update([{state, "active"}, {fail_reason, undefined}]),
              {reply, ok, dialing, State#state{session = NewSession}, timer:minutes(1)}
          end
      end;
    _ -> 
      {_, _, NewSession2} = finalize({failed, disabled}, State#state{session = NewSession}),
      {stop, normal, error, State#state{session = NewSession2}}
  end.

dialing({answer, Pbx}, State = #state{session_id = SessionId, session = Session = #session{call_log = CallLog}, resume_ptr = Ptr}) ->
  NewQueuedCall = Session#session.queued_call#queued_call{answered_at = {datetime, calendar:universal_time()}},
  NewQueuedCallSession = Session#session{queued_call = NewQueuedCall},

  error_logger:info_msg("Session (~p) answer", [SessionId]),
  CallLog:update([{started_at, calendar:universal_time()}]),

  monitor(process, Pbx:pid()),
  NewSession = NewQueuedCallSession#session{pbx = Pbx},
  notify_status('in-progress', NewSession),
  FlowPid = case Ptr of
    undefined -> spawn_run(NewSession, Ptr);
    _ ->
      RusumeSession = store_default_language(NewSession),
      spawn_run(RusumeSession, Ptr)
  end,

  {next_state, in_progress, State#state{pbx_pid = Pbx:pid(), flow_pid = FlowPid, session = NewSession}};

dialing({reject, Reason}, State = #state{session = Session = #session{session_id = SessionId, call_log = CallLog}}) ->
  error_logger:info_msg("Session (~p) rejected, reason: ~p", [SessionId, Reason]),
  CallLog:error(["Call was rejected. (Reason: ", atom_to_list(Reason),")"], []),
  notify_status('no-answer', Session),
  finalize({failed, Reason}, State);

dialing(timeout, State = #state{session = Session}) ->
  notify_status(busy, Session),
  finalize({failed, timeout}, State).

in_progress({completed, ok}, State = #state{session = Session}) ->
  notify_status(completed, Session),
  finalize(completed, State);

in_progress({completed, Failure}, State = #state{session = Session}) ->
  notify_status(failed, Session),
  finalize({failed, Failure}, State).
% in_progress({suspend, NewSessionPid, Ptr}, _From, State = #state{session = Session = #session{session_id = SessionId}}) ->

  % error_logger:info_msg("Session (~p) suspended", [SessionId]),

  % CallLog:update([{duration, answer_duration(Session)}, {state, <<"suspended">>}]),

  % channel_queue:unmonitor_session(Session#session.channel#channel.id, self()),
  % {reply, ok, ready, State#state{pbx_pid = undefined, flow_pid = undefined, resume_ptr = Ptr, session = NewSession}}.
in_progress({suspend, NewSessionPid, Ptr}, _From, State = #state{session = Session}) ->
  session:set_pointer(NewSessionPid, Ptr),
  
  channel_queue:unmonitor_session(Session#session.channel#channel.id, self()),
  
  notify_status(completed, Session),
  finalize(completed, State).

notify_status(Status, Session = #session{call_log = CallLog, address = Address, callback_params = CallbackParams}) ->
  case Session#session.status_callback_url of
    undefined -> ok;
    <<>> -> ok;
    Url ->
      CallSid = util:to_string(CallLog:id()),
      spawn(fun() ->
        Uri = uri:parse(binary_to_list(Url)),
        QueryString = [{"CallSid", CallSid}, {"CallStatus", Status}, {"From", Address} | CallbackParams],
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
  MatchResult = case Criteria of
    {project, ProjectId} ->
      Session#session.project#project.id == ProjectId;
    {channel, ChannelId} ->
      Session#session.channel#channel.id == ChannelId;
    {call_flow, CallFlowId} ->
      Session#session.call_flow#call_flow.id == CallFlowId;
    _ -> false
  end,
  {reply, MatchResult, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, Reason}, _, State = #state{pbx_pid = Pid}) ->
  {stop, Reason, State};

handle_info({'DOWN', _Ref, process, Pid, Reason}, _, State = #state{flow_pid = Pid}) ->
  {stop, Reason, State};

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%% @private
terminate(Reason, _, #state{session_id = Id, session = Session}) ->
  push_results(Session),
  error_logger:info_msg("Session (~p) terminated with reason: ~p", [Id, Reason]).

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

push_results(#session{call_flow = #call_flow{id = CallFlowId, store_in_fusion_tables = 1}, call_log = CallLog}) ->
  Task = ["--- !ruby/struct:CallFlow::FusionTablesPush::Pusher\ncall_flow_id: ", integer_to_list(CallFlowId),
    "\ncall_log_id: ", integer_to_list(CallLog:id()), "\n"],
  delayed_job:enqueue(Task);
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
  spawn_monitor(fun() ->
    try run(Session, Ptr) of
      % {suspend, NewSession, NewPtr} ->
      %   gen_fsm:sync_send_event(SessionPid, {suspend, NewSession, NewPtr});
      {suspend, NewSessionPid, NewPtr} ->
        gen_fsm:sync_send_event(SessionPid, {suspend, NewSessionPid, NewPtr});
      {{error, _}, NewSession} ->
        finalize({failed, error}, #state{session = NewSession});
      {Result, NewSession = #session{js_context = JsContext}} ->
        Status = erjs_context:get(status, JsContext),
        FlowResult = flow_result(Result, Status),
        case FlowResult of
          {error, "marked as failed"} ->
            notify_status(marked_as_failed, NewSession),
            finalize({failed, marked_as_failed}, #state{session = NewSession});
          _ -> gen_fsm:send_event(SessionPid, {completed, flow_result(Result, Status)})
        end
    after
      Pbx:terminate()
    end
  end).

flow_result(ok, "failed") -> {error, "marked as failed"};
flow_result({error, _}, "successful") -> ok;
flow_result(Result, _) -> Result.

get_contact(ProjectId, undefined, CallLogId) ->
  Address = "Anonymous" ++ integer_to_list(CallLogId),
  contact:create_anonymous(ProjectId, Address);
get_contact(ProjectId, Address, _) ->
  contact:find_or_create_with_address(ProjectId, Address).

default_variables(#session{contact = Contact, queued_call = QueuedCall, project = #project{id = ProjectId}, call_log = CallLog}) ->
  Context = erjs_context:new([{record_url, fun(Key) ->
    {ok, BaseUrl} = application:get_env(base_url),
    BaseUrl ++ "/calls/" ++ util:to_string(CallLog:id()) ++ "/results/" ++ util:to_string(Key)
  end}]),
  ProjectVars = project_variable:names_for_project(ProjectId),
  Variables = persisted_variable:find_all({contact_id, Contact#contact.id}),
  DefaultContext = default_variables(Context, ProjectVars, Variables),
  initialize_context(DefaultContext, QueuedCall).

initialize_context(Context, #queued_call{variables = Vars}) ->
  lists:foldl(fun({Name, Value}, C) ->
    case Value of
      undefined -> C;
      [] -> C;
      <<>> -> C;
      _ ->
        VarName = binary_to_atom(iolist_to_binary(["var_", Name]), utf8),
        erjs_context:set(VarName, Value, C)
    end
  end, Context, yaml_serializer:load(Vars));
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
run(Session = #session{flow = Flow, stack = Stack, call_log = CallLog}, Ptr) ->
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
        % suspend ->
        %   {suspend, NewSession, Ptr + 1}
        {suspend, NewSessionPid} ->
          {suspend, NewSessionPid, Ptr + 1};
        {suspend_reset_ptr, NewSessionPid} ->
          {suspend, NewSessionPid, undefined }
      end
  catch
    hangup ->
      CallLog:info("The user hang up", []),
      {hangup, Session};
    Class:Error ->
      CallLog:error(["Error ", io_lib:format("~p:~p", [Class, Error])], []),
      error_logger:error_msg("Error during session ~p: ~p:~p~n~p~n",
        [Session#session.session_id, Class, Error, erlang:get_stacktrace()]),
      {{error, {Class, Error}}, Session}
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
