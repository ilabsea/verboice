-module(reminder_group).
-export([has_address/2, register_address/2, deregister_address/2]).
-define(TABLE_NAME, "ext_reminder_groups").

-include_lib("erl_dbmodel/include/model.hrl").

-define(MAP, [
  {addresses, yaml_serializer}
]).

has_address(AddressBin, #reminder_group{addresses = AddrYaml}) ->
  Addresses = active_record_yaml:deserialize(AddrYaml),
  lists:member(binary_to_list(AddressBin), Addresses).

register_address(AddressBin, ReminderGroup = #reminder_group{addresses = AddrYaml}) ->
  case ReminderGroup:has_address(AddressBin) of
    true -> ReminderGroup;
    false ->
      Addresses = active_record_yaml:deserialize(AddrYaml),
      NewAddresses = lists:append(Addresses, [binary_to_list(AddressBin)]),
      ReminderGroup#reminder_group{addresses = active_record_yaml:serialize(NewAddresses)}
  end.

deregister_address(AddressBin, ReminderGroup = #reminder_group{addresses = AddrYaml}) ->
  case ReminderGroup:has_address(AddressBin) of
    true -> 
      Addresses = active_record_yaml:deserialize(AddrYaml),
      ReminderGroup#reminder_group{addresses = active_record_yaml:serialize(lists:delete(binary_to_list(AddressBin), Addresses))};
    false ->
      ReminderGroup
  end.