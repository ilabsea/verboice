-module(register).
-export([run/2]).

-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{project = Project, call_log = CallLog, js_context = JsContext}) ->
  ReminderGroupName = proplists:get_value(reminder_group, Args),
  Expr = proplists:get_value(number, Args),
  case reminder_group:find([{project_id, Project#project.id}, {name, ReminderGroupName}]) of
    undefined ->
      StepName = erjs_context:get(current_step_name, JsContext),
      throw("Step " ++ StepName ++ " is broken");
    ReminderGroup ->
      PhoneNumber = case Expr of
        [] -> 
          Call = call_log:find(CallLog:id()),
          Call:address_without_prefix();
        X -> 
          {OtherNumber, _} = erjs:eval(X, JsContext),
          OtherNumber
      end,
      
      NewReminderGroup = ReminderGroup:register_address(PhoneNumber),
      NewReminderGroup:save(),

      poirot:log(info, "has been registered to ~p", [ReminderGroupName]),
      CallLog:info([PhoneNumber, " has been registered to ", ReminderGroupName], [])
  end,
 
  {next, Session}.
