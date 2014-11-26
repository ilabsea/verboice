-module(admin).

-export([notify_session_timeout/1]).

-include("session.hrl").
-include("db.hrl").

notify_session_timeout(Session) ->
  {ok, Recipient} = application:get_env(recipient),
  {ok, Sender} = application:get_env(sender),

  case sendmail:send(util:to_string(Recipient),
                     util:to_string(Sender),
                     util:to_string(subject_message()),
                     util:to_string(body_message(Session))) of
    {0, _} -> {info, "Email sent"};
    _ -> {error, "Email can't be sent"}
  end.

subject_message() ->
  {ok, Instance} = application:get_env(hostname),
  string:join(["NoACK", Instance], "-").

body_message(#session{channel = Channel, call_log = CallLog}) ->
  Call = call_log:find(CallLog:id()),

  string:join([string:concat("channel_id:", util:to_string(Channel#channel.id)),
    string:concat("channel_number:", util:to_string(Channel:number())),
    string:concat("caller_id:", util:to_string(Call#call_log.address)),
    string:concat("direction:", util:to_string(Call#call_log.direction)),
    string:concat("datetime:", datetime_utils:strftime(Call:called_at()))
  ], ",").