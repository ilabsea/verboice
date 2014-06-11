-module(record).
-export([run/2, create_call_log_recorded_audio/6]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, call_log = CallLog, contact = Contact, project = Project}) ->
  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),
  OldVarName = proplists:get_value(old_var_name, Args),
  VarName = proplists:get_value(var_name, Args),
  StopKeys = proplists:get_value(stop_keys, Args, "01234567890*#"),
  Timeout = proplists:get_value(timeout, Args, 10),
  SilenceTime = proplists:get_value(silence_detection, Args),

  CallLog:info("Record user voice", [{command, "record"}, {action, "start"}]),
  CallLogId = CallLog:id(),
  Filename = filename(CallLogId, Key),
  filelib:ensure_dir(Filename),

  Pbx:record(Filename, StopKeys, Timeout, SilenceTime),

  RecordedAudio = #recorded_audio{
    contact_id = Contact#contact.id,
    project_id = Project#project.id,
    call_log_id = CallLogId,
    key = Key,
    description = Description
  },
  RecordedAudio:save(),

  create_call_log_recorded_audio(OldVarName, VarName, Key, Description, Project#project.id, CallLogId),

  CallLog:info("Recording saved", [{command, "record"}, {action, "finish"}]),
  {next, Session}.

filename(CallLogId, Key) ->
  filename:join(["../data/call_logs/", util:to_string(CallLogId), "results", Key ++ ".wav"]).

create_call_log_recorded_audio(OldVarName, VarName, Key, Description, ProjectId, CallLogId) ->
  case VarName of
    undefined -> io:format("do nothing");
    _ ->
      ExistingProjectVariables = project_variable:find_all([{project_id, ProjectId}, {name, OldVarName}]),
      ProjectVariableOld = case length(ExistingProjectVariables) of
        0 -> undefined;
        _ -> lists:last(ExistingProjectVariables)
      end,

      ProjectVaraibles = project_variable:find_all([{project_id, ProjectId}, {name, VarName}]),
      ProjectVariableCur = case length(ProjectVaraibles) of
        0 -> undefined;
        _ -> lists:last(ProjectVaraibles)
      end,

      ProjectVariable = case ProjectVariableOld of
        undefined -> 
          case ProjectVariableCur of
            undefined ->
              NewProjectVariable = #project_variable{project_id = ProjectId, name = VarName},
              NewProjectVariable:save() ;
            _ -> ProjectVariableCur
          end;
        _ -> 
          if 
            OldVarName /= VarName -> ProjectVariableOld:update([{name, VarName}]);
            true -> io:format("do nothing")
          end,
          ProjectVariableOld
      end,

      ProjectVariableId = ProjectVariable#project_variable.id,

      CallLogRecordedAudio = call_log_recorded_audio:find([ {call_log_id, CallLogId}, 
                                                            {project_variable_id, ProjectVariableId }
                                                          ]),
      case CallLogRecordedAudio of
        undefined ->
          NewCallLogRecordedAudio = #call_log_recorded_audio{
            call_log_id = CallLogId,
            project_variable_id = ProjectVariableId,
            key = Key,
            description = Description
          },
          NewCallLogRecordedAudio:save();
        _ -> 
          CallLogRecordedAudio:update([ {call_log_id, CallLogId}, 
                                        {project_variable_id, ProjectVariableId}, 
                                        {key, Key}, 
                                        {description, Description}
                                      ])
      end
    end.