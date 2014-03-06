-module(register).
-export([run/2]).

-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{project = Project, address = Address, call_log = CallLog, js_context = JsContext}) ->
  GroupName = proplists:get_value(reminder_group, Args),
  Number = proplists:get_value(number, Args),
  case reminder_group:find([{project_id, Project#project.id}, {name, GroupName}]) of
    undefined ->
      StepName = erjs_context:get(current_step_name, JsContext),
      throw("Step " ++ StepName ++ " is broken");
    Group ->
      NewNumber = list_to_binary(Number),
      PhoneNumber = case NewNumber of
        <<>> -> Address;
        X -> 
          {Value, _} = erjs:eval(X, JsContext),
          NewValue = list_to_binary(Value),
          NewValue
      end,
      
      NewGroup = Group:register_address(PhoneNumber),
      NewGroup:save(),
      CallLog:info([PhoneNumber, " has been registered to ", GroupName], [])
  end,
 
  {next, Session}.