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
    Contact ->
      Addresses = [util:to_string(ContactAddress#contact_address.address) || ContactAddress <- contact_address:find_all([{contact_id, Contact#contact.id}])],
      CallLog:info(io_lib:format("Impersonating as contact: ~p", [Addresses]), []),
      impersonate(Session, Contact)
  end,
  {next, NewSession}.

impersonate(Session, Contact) ->
  NewSession = Session#session{contact = Contact},
  Context = session:default_variables(NewSession),
  Context2 = erjs_context:set(impersonated, true, Context),
  NewSession#session{js_context = Context2}.
