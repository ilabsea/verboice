-module(register).
-export([run/2]).

-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{project = Project, call_log = CallLog, js_context = JsContext}) ->
  GroupName = proplists:get_value(reminder_group, Args),
  Number = proplists:get_value(number, Args),
  case reminder_group:find([{project_id, Project#project.id}, {name, GroupName}]) of
    undefined ->
      StepName = erjs_context:get(current_step_name, JsContext),
      throw("Step " ++ StepName ++ " is broken");
    Group ->
      PhoneNumber = case list_to_binary(Number) of
        <<>> -> 
          Call = call_log:find(CallLog:id()),
          Call:address_without_prefix();
        X -> 
          {Value, _} = erjs:eval(X, JsContext),
          OtherNumber = list_to_binary(Value),
          OtherNumber
      end,
      
      NewGroup = Group:register_address(PhoneNumber),
      NewGroup:save(),
      CallLog:info([PhoneNumber, " has been registered to ", GroupName], [])
  end,
 
  {next, Session}.