-module(deregister).
-export([run/2]).

-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{project = Project, address = Address, call_log = CallLog, js_context = JsContext}) ->
  GroupName = proplists:get_value(reminder_group, Args),
  case GroupName of
    undefined ->
      StepName = erjs_context:get(current_step_name, JsContext),
      throw("Step " ++ StepName ++ " is broken");
    "All" ->
      Groups = reminder_group:find_all([{project_id, Project#project.id}]),
      lists:foreach(
        fun(Group) ->
          ExistingGroup = Group:deregister_address(Address),
          ExistingGroup:save()
        end,
        Groups
      ),
      CallLog:info([Address, " has been deregistered from every reminders under project: ", Project#project.name], []);
    TheGroupName ->
      case reminder_group:find([{project_id, Project#project.id}, {name, TheGroupName}]) of
        undefined ->
          StepName = erjs_context:get(current_step_name, JsContext),
          throw("Step " ++ StepName ++ " is broken");
        Group ->
          ExistingGroup = Group:deregister_address(Address),
          ExistingGroup:save(),
          CallLog:info([Address, " has been deregistered from ", TheGroupName], [])
      end
    end,
 
  {next, Session}.