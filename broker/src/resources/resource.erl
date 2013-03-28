-module(resource).
-export([prepare/2]).

prepare(Guid, Pbx) ->
  case get_resource(Guid) of
    {text, Text} -> prepare_text_resource(Text, Pbx)
  end.

get_resource(Guid) ->
  [Id] = db:select_one("SELECT id FROM resources WHERE guid = ~p", [Guid]),
  [Type, Text] = db:select_one("SELECT type, text FROM localized_resources WHERE resource_id = ~p AND language = 'es'", [Id]),

  case Type of
    <<"TextLocalizedResource">> -> {text, Text}
  end.

prepare_text_resource(Text, Pbx) ->
  Hash = crypto:md5(Text),
  Name = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]),

  TargetPath = Pbx:sound_path_for(Name),
  case filelib:is_file(TargetPath) of
    true -> ok;
    false -> synthesize(Text, TargetPath)
  end,
  Name.

synthesize(Text, TargetPath) ->
  TempFile = TargetPath ++ ".wave",
  try
    Port = open_port({spawn, "say -v Paulina -o " ++ TempFile}, [binary]),
    port_command(Port, Text),
    port_close(Port),
    ok = wait_for_file(TempFile),
    os:cmd("sox " ++ TempFile ++ " -r 8000 -c1 " ++ TargetPath)
  after
    file:delete(TempFile)
  end.


wait_for_file(FilePath) -> wait_for_file(FilePath, 5).
wait_for_file(_, 0) -> timeout;
wait_for_file(FilePath, N) ->
  case filelib:is_file(FilePath) of
    true -> ok;
    false -> timer:sleep(50), wait_for_file(FilePath, N - 1)
  end.