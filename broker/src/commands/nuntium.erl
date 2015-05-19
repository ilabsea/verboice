-module(nuntium).
-export([run/2, can_play/1]).
-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

-define(QST_SERVER, "qst_server").
-define(SMTP, "smtp").

run(Args, Session = #session{call_log = CallLog, project = Project}) ->
  Kind = proplists:get_value(kind, Args, undefined),
  RcptType = proplists:get_value(rcpt_type, Args),
  Expr = proplists:get_value(expr, Args),
  SubjectGuid = proplists:get_value(subject_guid, Args),
  ResourceGuid = proplists:get_value(resource_guid, Args),
  ChannelId = proplists:get_value(channel_id, Args),

  {Result, Message} = case rcpt_address(Kind, RcptType, Expr, Session) of
    undefined -> {error, "Missing recipient"};
    RecipientAddress ->
      case sender_address(Kind) of
        undefined -> {error, "Missing sender address"};
        SenderAddress ->
          case message(SubjectGuid, Session) of
            error -> {error, "Missing subject"};
            Subject ->
              case resource:prepare(ResourceGuid, Session#session{pbx = nuntium}) of
                {text, _Lang, Body} ->
                  case Kind of
                    undefined -> {error, "Missing channel type"};
                    _ ->
                      NuntiumArgs = [
                        {from, SenderAddress},
                        {to, RecipientAddress},
                        {subject, Subject},
                        {body, Body},
                        {account_id, Project#project.account_id}
                      ],
                      NuntiumArgs1 = case channel_name(ChannelId) of
                        undefined -> NuntiumArgs;
                        ChannelName -> [{suggested_channel, ChannelName} | NuntiumArgs]
                      end,
                      poirot:log(debug, "Sending to nuntium: ~p", [NuntiumArgs1]),
                      case nuntium_api:send_ao(NuntiumArgs1) of
                        ok -> {info, "Sent"};
                        {error, Reason} -> {error, Reason}
                      end
                  end;
                _ -> {error, "Missing text to send"}
              end
          end
      end
  end,

  case Result of
    info -> poirot:log(info, Message);
    error -> poirot:log(error, Message)
  end,

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

channel_name(ChannelId) ->
  case ChannelId of
    undefined -> undefined;
    _ ->
      Channel = nuntium_channel:find(ChannelId),
      case Channel of
        undefined -> undefined;
        _ -> Channel#nuntium_channel.channel_name
      end
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

sender_address(Kind) ->
  case Kind of
    ?QST_SERVER -> <<"sms://verboice">>;
    ?SMTP -> <<"noreply@verboice.org">>;
    _ -> undefined
  end.

message(Guid, Session) ->
  case Guid of
    undefined -> <<>>;
    "" -> <<>>;
    _ -> case resource:prepare(Guid, Session#session{pbx = nuntium}) of
      {text, _Lang, Subject} -> Subject;
      _ -> error
    end
  end.

can_play({text, _}) -> true;
can_play(_) -> false.
