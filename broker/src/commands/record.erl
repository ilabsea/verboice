-module(record).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, call_log = CallLog, contact = Contact, project = Project}) ->
  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),
  OldVarName = proplists:get_value(old_var_name, Args),
  VarName = proplists:get_value(var_name, Args),
  StopKeys = proplists:get_value(stop_keys, Args, "01234567890*#"),
  Timeout = proplists:get_value(timeout, Args, 10),

  CallLog:info("Record user voice", [{command, "record"}, {action, "start"}]),
  CallLogId = CallLog:id(),
  Filename = filename(CallLogId, Key),
  filelib:ensure_dir(Filename),

  Pbx:record(Filename, StopKeys, Timeout),

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
  {ok, RecordDir} = application:get_env(record_dir),
  filename:join([RecordDir, util:to_string(CallLogId), "results", Key ++ ".wav"]).

create_call_log_recorded_audio(OldVarName, VarName, Key, Description, ProjectId, CallLogId) ->
  OldVarNameBin = list_to_binary(OldVarName),
  VarNameBin = list_to_binary(VarName),

  if
    OldVarNameBin /= <<>>; VarNameBin /= <<>> ->
      ProjectVariableOld = project_variable:find([{project_id, ProjectId}, {name, OldVarName}]),
      ProjectVariableCur = project_variable:find([{project_id, ProjectId}, {name, VarName}]),

      ProjectVariable = case ProjectVariableOld  of
        undefined -> 
          if 
            ProjectVariableCur == undefined ->    
              NewProjectVariable = #project_variable{project_id = ProjectId, name = VarName},
              NewProjectVariable:save() ;
            true -> 
              ProjectVariableCur      
          end;  
        _ -> 
          if OldVarName  /= OldVarNameBin -> ProjectVariableOld:update([{name, VarName}]) end,
          ProjectVariableOld
      end,

      ProjectVariableId = ProjectVariable#project_variable.id ,

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
      end;
    true -> io:format("Do nothing", [])
  end.
