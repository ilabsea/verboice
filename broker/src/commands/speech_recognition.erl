-module(speech_recognition).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, js_context = JS, call_log = CallLog, contact = Contact, project = Project}) ->
  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),
  StopKeys = proplists:get_value(stop_keys, Args, "01234567890*#"),
  Timeout = proplists:get_value(timeout, Args, 10),
  SilenceDetection = proplists:get_value(silence_detection, Args),


  Store     = proplists:get_value(store, Args),
  OldStore  = proplists:get_value(old_store, Args),

  Result1   = proplists:get_value(result1, Args),
  Result2   = proplists:get_value(result2, Args),
  Result3   = proplists:get_value(result3, Args),

  Accuracy1 = proplists:get_value(accuracy1, Args),
  Accuracy2 = proplists:get_value(accuracy2, Args),
  Accuracy3 = proplists:get_value(accuracy3, Args),

  MinConfidence = proplists:get_value(min_confidence, Args),
  
  VariableList = { {Result1, Accuracy1}, {Result2, Accuracy2}, {Result3, Accuracy3} },

  CallLog:info("Record user voice", [{command, "record"}, {action, "start"}]),
  CallLogId = CallLog:id(),
  Filename = filename(CallLogId, Key),
  filelib:ensure_dir(Filename),

  Pbx:record(Filename, StopKeys, Timeout, SilenceDetection),

  RecordedAudio = #recorded_audio{
    contact_id = Contact#contact.id,
    project_id = Project#project.id,
    call_log_id = CallLogId,
    key = Key,
    description = Description
  },
  RecordedAudio:save(),
  record:create_call_log_recorded_audio(OldStore, Store, Key, Description, Project#project.id, CallLogId),

  CallLog:info("Speech recognition running os command", [{command, "speech_recognition"}, {action, "decoding"}]),

  SpeechDecode = decode_audio_speech(Filename, CallLogId),
  Confidence1Value = store_result_from_speech(MinConfidence, SpeechDecode, VariableList, Session),

  LogInfo = "Speech recognition given confidence: " ++ float_to_list(Confidence1Value, [{decimals,2}]) ++ ", min confidence required: " ++ MinConfidence,
  CallLog:info(LogInfo, [{command, "speech_recognition"}, {action, "comparing value"}]),
  CallLog:info("Speech recognition finished", [{command, "speech_recognition"}, {action, "finish"}]),
  CallLog:info("Recording saved", [{command, "speech_recognition"}, {action, "finish"}]),

  JS2 = erjs_context:set(confidence1, Confidence1Value, JS),  
  {next, Session#session{js_context = JS2}}.

decode_audio_speech(Filename, CallLogId) ->
  Command     = resource_os_command(Filename, CallLogId),
  list_to_binary(os:cmd(Command)).

% resource_os_command_dev(Filename) ->
%   {ok, Path}   = file:get_cwd() ,
%   FileFullPath = Path  ++ "/../" ++ "script/speech_recognition.php",
%   io:format("~n File name is : ~p", [Filename] ),
%   "php " ++ FileFullPath.

resource_os_command(Filename, CallLogId) ->
  {ok, Path}   = file:get_cwd() ,
  WorkingPath  = Path  ++ "/" ++ "../" ,
  FileFullPath = Path  ++ "/" ++ Filename,
  Sphinx3Path = WorkingPath ++ "script/sphinx3/OPEN/decode" ,
  "cd " ++  Sphinx3Path ++ " && ./recognizer-nbest.csh " ++ FileFullPath ++ " " ++ integer_to_list(CallLogId).


filename(CallLogId, Key) ->
  filename:join(["../data/call_logs/", util:to_string(CallLogId), "results", Key ++ ".wav"]).

store_result_from_speech(MinConfidence,SpeechDecode, VariableList, Session) ->
  Confidence1Error = -1 ,
  Speech = json:decode(SpeechDecode),
         % {ok,{
         %      [
         %        {<<"results">>, 
         %          [ 
         %            {
         %              [ 
         %                {<<"result">>,<<"kompong cham">>},
         %                {<<"confidence">>,85.5 }
         %              ]
         %            },

         %            { [ 
         %                {<<"result">>,<<"kompong chnaing">>},
         %                {<<"confidence">>,70.23}
         %              ]
         %            },

         %            { [ 
         %                {<<"result">>,<<"kompong thom">>},
         %                {<<"confidence">>,40.31}]}
         %              ]
         %            },
         %        {<<"error">>,<<>>}
         %      ]
         %     }
         %  }
  % io:format(" ~n json decode from speech recognition ~p", [Speech]),
  if element(1, Speech) == ok ->
    { _, { [ { _ , ResultList }, { _ , Error} ] } } = Speech ,
      if Error /= <<>> ->
        #session{call_log = CallLog} = Session ,
        CallLog:info("Speech recognition raise error with:" ++ Error, [{command, "speech_recognition"}, {action, "convert_sound"}]);
      true ->  
        WorkingResultList = lists:sublist(ResultList, 1, 3), % we are only interested in 3 first elements
        store_list_elements( WorkingResultList , VariableList, 1, Session),
        MinConfidence, % io:format("~n Min confidence is: ~p ", [MinConfidence]),
        get_first_confidence_value(WorkingResultList)

        % if FirstConfidence > MinConfidence ->
        %    store_list_elements( WorkingResultList , VariableList, 1, Session);
        % true ->
        %    io:format(" ~n Confidence received is: ~p, required min confidence: ~p", [ FirstConfidence, MinConfidence ])
        % end,    
        % FirstConfidence
      end;  
  true ->
    #session{call_log = CallLog} = Session ,
    CallLog:info("Invalid JSon format from speech recognition:" ++ SpeechDecode, [{command, "speech_recognition"}, {action, "json_error"}]), 
    Confidence1Error
  end.

get_first_confidence_value(WorkingResultList) ->
  [FirstElement | _ ] = WorkingResultList,
  { [ _ , { _ , FirstConfidence }]} = FirstElement,
  FirstConfidence + 0.0 .

store_element(Element, VarList, Index, Session) ->
  {ResultVar, ConfidenceVar} = element(Index, VarList), 
  {[{_, Result},{_, Confidence}]} = Element,

  store_result_data(ResultVar, Result, Session),
  store_result_data(ConfidenceVar,Confidence, Session).

store_list_elements([Element], VariableList, N, Session) ->
  store_element(Element, VariableList, N, Session);

store_list_elements([Element|T], VariableList, N, Session) ->
  store_element(Element, VariableList, N, Session),
  store_list_elements(T, VariableList, N+1, Session).


store_result_data("", _ , _ ) -> nothing ;
store_result_data(VarName, Value, Session) ->
  #session{project = MapProject} = Session,
  ProjectId = MapProject#project.id,
  ProjectVar  = project_variable:find_or_create([{project_id, ProjectId}, {name, VarName}]),

  if ProjectVar == undefined ->
     NewProjectVariableName = #project_variable{project_id=ProjectId, name=VarName},
     NewProjectVariableName:save();
  true ->
     nothing
  end,
  persist_variable:store_peristed_variable(VarName, Value, Session).