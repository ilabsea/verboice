-module(personate).
-export([run/2]).

-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{project = Project, call_log = CallLog, js_context = JS}) ->
  Expr = proplists:get_value(number, Args),
  {Address, JS2} = erjs:eval(Expr, JS),

  NewSession = case contact:find_by(Project#project.id, Address) of
    undefined ->
      CallLog:info(io_lib:format("Could not impersonate. Contact not found with address: ~p ", [Address]), []),
      Session#session{js_context = erjs_context:set(impersonated, false, JS2)};
    {Contact, ContactAddress} ->
      CallLog:info(io_lib:format("Impersonating as contact: ~p", [ContactAddress#contact_address.address]), []),
      impersonate(Session, Contact)
  end,
  {next, NewSession}.

impersonate(Session, Contact) ->
  NewSession = Session#session{contact = Contact},
  % Context = session:default_variables(NewSession),

  DefaultContext=session:merge(NewSession, Session#session.js_context),
  NewContext = erjs_context:set(impersonated, true, DefaultContext),
  NewSession#session{js_context = NewContext}.
