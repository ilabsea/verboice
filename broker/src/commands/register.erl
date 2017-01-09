-module(register).
-export([run/2]).

-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{project = Project, call_log = CallLog, js_context = JsContext}) ->
  GroupName = proplists:get_value(reminder_group, Args),
  Expr = proplists:get_value(number, Args),

  StepName = erjs_context:get(current_step_name, JsContext),

  case reminder_group:find([{project_id, Project#project.id}, {name, GroupName}]) of
    undefined ->
      raise_error(StepName, " is broken");
    Group ->
      PhoneNumber = case Expr of
        [] ->
          Call = call_log:find(CallLog:id()),
          Call:address_without_prefix();
        X ->
          {OtherNumber, _} = erjs:eval(X, JsContext),
          OtherNumber
      end,

      case PhoneNumber of
        [] ->
          raise_error(StepName, "is missing phone number");
        null ->
          raise_error(StepName, "is missing phone number");
        _ ->
          NewGroup = Group:register_address(PhoneNumber),
          NewGroup:save(),

          poirot:log(info, "has been registered to ~p", [GroupName]),
          CallLog:info([PhoneNumber, " has been registered to ", GroupName], [])
      end
  end,

  {next, Session}.

raise_error(StepName, FailReason) ->
  error_logger:info_msg(StepName ++ FailReason),
  poirot:add_meta([{error, list_to_binary(StepName ++ FailReason)}]),
  throw(FailReason).

