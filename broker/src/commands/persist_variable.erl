-module(persist_variable).
-export([run/2, get_current_date/0, store_peristed_variable/3 ]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Name = proplists:get_value(name, Args),
  Expression = proplists:get_value(expression, Args),
  Type = proplists:get_value(type, Args),
  {Value, JS2} = erjs:eval(Expression, JS),

  TypeBin = list_to_binary(Type),

  NewValue = case TypeBin of
    <<>> -> Value;
    <<"CurrentDate">> -> 
      get_current_date();    
    UnitBin ->
      Today = erlang:date(),
      NewDate = time_ago(Value, Today, UnitBin),
      date_to_string_format(NewDate)
  end,

  store_peristed_variable(Name, NewValue, Session), % return a PersistedVar
  % create_session_call_log_variable(CallLog:id(), PersistedVar#persisted_variable.project_variable_id, NewValue),

  VarName = list_to_atom("var_" ++ Name),
  JS3 = erjs_context:set(VarName, NewValue, JS2),
  {next, Session#session{js_context = JS3}}.

store_peristed_variable(Name, NewValue, Session) ->
  StorePersistedVar = (find_or_create_persisted_variable(Name, Session))#persisted_variable{value = NewValue},
  StorePersistedVar:save(),
  #session{call_log = CallLog } = Session,
  create_session_call_log_variable(CallLog:id(), StorePersistedVar#persisted_variable.project_variable_id, NewValue),
  StorePersistedVar.

get_2_digits_string(Value) ->
  TwoDigit = if  
    Value < 10 -> "0" ++ integer_to_list(Value);
    true -> integer_to_list(Value)
   end,
   TwoDigit.

get_current_date() ->
  {{Year,Month,Day}, _} = calendar:universal_time(),
  DatePart = get_2_digits_string(Day)++ "/" ++ get_2_digits_string(Month) ++ "/" ++ integer_to_list(Year),
  list_to_binary(DatePart).

date_to_string_format(Date) ->
  {Year, Month, Day} = Date,
  DateString = get_2_digits_string(Day)++ "/" ++ get_2_digits_string(Month) ++ "/" ++ integer_to_list(Year),
  list_to_binary(DateString).



find_or_create_persisted_variable(Name, #session{contact = Contact, project = Project}) ->
  case Name of
    "language" ->
      persisted_variable:find_or_new([
        {contact_id, Contact#contact.id},
        {implicit_key, "language"}
      ]);
    _ ->
      #project_variable{id = ProjectVarId} = project_variable:find([{project_id, Project#project.id}, {name, Name}]),
      persisted_variable:find_or_new([
        {contact_id, Contact#contact.id},
        {project_variable_id, ProjectVarId}
      ])
  end.

create_session_call_log_variable(CallLogId, ProjectVariableId, Value) ->
  CallLogVariable = (call_log_variable:find_or_new([{call_log_id, CallLogId}, {project_variable_id, ProjectVariableId}]))#call_log_variable{value = Value},
  CallLogVariable:save().

time_ago(NumberAgo, FromDate = {_, _, _}, UnitBin) ->
  NewDate = case UnitBin of
    <<>> -> erlang:localtime();
    <<"Day">> -> edate:shift(FromDate, -list_to_integer(NumberAgo), day);
    <<"Week">> -> edate:shift(FromDate, -list_to_integer(NumberAgo), week);
    <<"Month">> -> edate:shift(FromDate, -list_to_integer(NumberAgo), month);
    <<"Year">> -> edate:shift(FromDate, -list_to_integer(NumberAgo), year)
  end,

  NewDate.