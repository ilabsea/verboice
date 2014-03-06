-module(reminder_group).
-export([has_address/2, register_address/2, deregister_address/2]).
-define(TABLE_NAME, "ext_reminder_groups").

-include_lib("erl_dbmodel/include/model.hrl").

-define(MAP, [
  {addresses, yaml_serializer}
]).

has_address(AddressBin, #reminder_group{addresses = AddrYaml}) ->
  Addresses = yaml_serializer:load(AddrYaml),
  Address = binary_to_list(AddressBin),
  case lists:member(Address, Addresses) of
    true -> true;
    _ ->
      % TODO refactoring
      % active record serialize attribute didn't wrapper text with single quote so it suppose to be integer
      AddressInt = binary_to_integer(AddressBin),
      lists:member(AddressInt, Addresses)
  end.

register_address(AddressBin, ReminderGroup = #reminder_group{addresses = AddrYaml}) ->
  case ReminderGroup:has_address(AddressBin) of
    true -> ReminderGroup;
    false ->
      Addresses = yaml_serializer:load(AddrYaml),
      Address = binary_to_list(AddressBin),
      NewAddresses = lists:append(Addresses, [Address]),
      ReminderGroup#reminder_group{addresses = active_record_yaml:serialize(NewAddresses)}
  end.

deregister_address(AddressBin, ReminderGroup = #reminder_group{addresses = AddrYaml}) ->
  case ReminderGroup:has_address(AddressBin) of
    true -> 
      Addresses = yaml_serializer:load(AddrYaml),
      Address = binary_to_list(AddressBin),
      ReminderGroup#reminder_group{addresses = active_record_yaml:serialize(lists:delete(Address, Addresses))};
    false ->
      ReminderGroup
  end.