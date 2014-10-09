-module(admin).

-export([notify_session_cleanup/0]).
-define(RECIPIENT, ["kakada@instedd.org"]).
-define(SENDER, "noreply@verboice.instedd.org").

notify_session_cleanup() ->
  case sendmail:send(util:to_string(?RECIPIENT),
                     util:to_string(?SENDER),
                     util:to_string("clean session"),
                     util:to_string("clean session")) of
    {0, _} -> {info, "Email sent"};
    _ -> {error, "Email can't be sent"}
  end.