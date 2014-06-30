-module(nuntium).
-export([run/2, can_play/1]).
-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

-define(QST_SERVER, "qst_server").
-define(SMTP, "smtp").

run(Args, Session = #session{call_log = CallLog, project = Project}) ->
  Kind = proplists:get_value(kind, Args),
  RcptType = proplists:get_value(rcpt_type, Args),
  Expr = proplists:get_value(expr, Args),
  SubjectGuid = proplists:get_value(subject_guid, Args),
  ResourceGuid = proplists:get_value(resource_guid, Args),

  CallLog:info(["Send text message '", ResourceGuid, "'"], [{command, "nuntium"}, {action, "start"}]),

  {Result, Message} = case rcpt_address(Kind, RcptType, Expr, Session) of
    undefined -> {error, "Missing recipient"};
    RecipientAddress ->
      case sender_address(Kind, Session) of
        undefined -> {error, "Missing sender address"};
        SenderAddress ->
          case subject_message(SubjectGuid, Session) of
            error -> {error, "Missing subject"};
            Subject -> 
              case body_message(ResourceGuid, Session) of
                undefined -> {error, "Missing text to send"};
                error -> {error, "Can't play text message"};
                Body ->
                  case Kind of
                    ?QST_SERVER ->
                      case nuntium_channel:find([{default, 1}, {account_id, Project#project.account_id}]) of
                        undefined -> {error, "No default SMS channel available"};
                        DefaultChannel -> 
                          NuntiumArgs = [
                            {from, SenderAddress},
                            {to, RecipientAddress},
                            {subject, Subject},
                            {body, Body},
                            {account_id, Project#project.account_id},
                            {suggested_channel, DefaultChannel#nuntium_channel.channel_name}
                          ],
                          case nuntium_api:send_ao(NuntiumArgs) of
                            ok -> {info, "SMS sent"};
                            {error, Reason} -> {error, Reason}
                          end
                      end;
                    ?SMTP -> 
                      case sendmail:send(util:to_string(RecipientAddress), util:to_string(SenderAddress), util:to_string(Subject), util:to_string(Body)) of
                        {0, _} -> {info, "Email sent"};
                        _ -> {error, "Email can't be sent"}
                      end;
                    _ -> {error, "Missing channel type"}
                  end
              end
          end
      end
  end,

  CallLog:Result(["Result: ", Message], []),

  {next, Session}.

rcpt_address(Kind, RcptType, Expr, Session) ->
  case rcpt_address_from_session(RcptType, Expr, Session) of
    undefined -> undefined;
    "" -> undefined;
    Address -> recipient(Kind, Address)
  end.

rcpt_address_from_session(caller, _, #session{js_context = JS, address = Address}) ->
  case erjs_context:get(var_sms_number, JS) of
    undefined -> Address;
    "" -> Address;
    Number -> list_to_binary(Number)
  end;

rcpt_address_from_session(expr, Expr, #session{js_context = JS}) ->
  {Value, _} = erjs:eval(Expr, JS),
  case Value of
    null -> undefined;
    _ -> list_to_binary(util:to_string(Value))
  end.

recipient(Kind, Address) ->
  case Kind of
    ?QST_SERVER ->
      case binary:match(Address, <<"://">>) of
        nomatch -> <<"sms://", Address/binary>>;
        _ -> Address
      end;
    ?SMTP -> Address;
    _ -> undefined
  end.

sender_address(Kind, #session{call_log = CallLog}) ->
  case Kind of
    ?QST_SERVER -> <<"sms://verboice">>;
    ?SMTP -> 
      Call = call_log:find(CallLog:id()),
      case account:find(Call#call_log.account_id) of
        undefined -> <<"noreply@verboice.org">>;
        Account -> Account#account.email
      end;
    _ -> undefined
  end.

subject_message(Guid, Session) ->
  case Guid of
    undefined -> <<>>;
    "" -> <<>>;
    _ -> case resource:prepare(Guid, Session#session{pbx = nuntium}) of
      {text, _Lang, Subject} -> Subject;
      _ -> error
    end
  end.

body_message(Guid, Session) ->
  case Guid of
    undefined -> undefined;
    "" -> undefined;
    _ -> case resource:prepare(Guid, Session#session{pbx = nuntium}) of
        {text, _Lang, Body} -> Body;
        _ -> error
    end
  end.

can_play({text, _}) -> true;
can_play(_) -> false.
