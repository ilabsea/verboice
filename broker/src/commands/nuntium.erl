-module(nuntium).
-export([run/2, can_play/1]).
-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

run(Args, Session = #session{call_log = CallLog, project = Project}) ->
  Kind = proplists:get_value(kind, Args),
  RcptType = proplists:get_value(rcpt_type, Args),
  Expr = proplists:get_value(expr, Args),
  SubjectGuid = proplists:get_value(subject_guid, Args),
  ResourceGuid = proplists:get_value(resource_guid, Args),
  NuntiumChannelId = proplists:get_value(nuntium_channel_id, Args),
  NuntiumChannel = nuntium_channel:find(NuntiumChannelId),

  CallLog:info(["Send text message '", ResourceGuid, "'"], [{command, "nuntium"}, {action, "start"}]),

  {Result, Message} = case rcpt_address(Kind, RcptType, Expr, Session) of
    undefined -> {error, "Missing recipient"};
    RecipientAddress ->
      case sender_address(Kind) of
        undefined -> {error, "Missing kind"};
        SenderAddress ->
          case subject_message(SubjectGuid, Session) of
            error -> {error, "Missing subject"};
            Subject -> 
              case body_message(ResourceGuid, Session) of
                undefined -> {error, "Missing text to send"};
                error -> {error, "Can't play text message"};
                Body ->
                  NuntiumArgs = [
                    {from, SenderAddress},
                    {to, RecipientAddress},
                    {subject, Subject},
                    {body, Body},
                    {account_id, Project#project.account_id},
                    {suggested_channel, NuntiumChannel#nuntium_channel.channel_name}
                  ],
                  case nuntium_api:send_ao(NuntiumArgs) of
                    ok -> {info, "Sent"};
                    {error, Reason} -> {error, Reason}
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
    "qst_server" ->
      case binary:match(Address, <<"://">>) of
        nomatch -> <<"sms://", Address/binary>>;
        _ -> Address
      end;
    "smtp" ->
      case binary:match(Address, <<"://">>) of
        nomatch -> <<"mailto://", Address/binary>>;
        _ -> Address
      end;
    _ -> undefined
  end.

sender_address(Kind) ->
  case Kind of
    "qst_server" -> <<"sms://verboice">>;
    "smtp" -> <<"mailto://verboice">>;
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
