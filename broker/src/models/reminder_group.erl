-module(reminder_group).
-export([has_address/2, register_address/2, deregister_address/2]).
-define(TABLE_NAME, "ext_reminder_groups").

-include_lib("erl_dbmodel/include/model.hrl").

-define(MAP, [
  {addresses, yaml_serializer}
]).

has_address(Address, ReminderGroup) when is_binary(Address) -> has_address(binary_to_list(Address), ReminderGroup);
has_address(Address, ReminderGroup) when is_integer(Address) -> has_address(integer_to_list(Address), ReminderGroup);
has_address(Address, #reminder_group{addresses = AddrYaml}) -> 
  Addresses = active_record_yaml:deserialize(AddrYaml),
  address:exist(Address, Addresses).

register_address(Address, ReminderGroup) when is_binary(Address) -> register_address(binary_to_list(Address), ReminderGroup);
register_address(Address, ReminderGroup) when is_integer(Address) -> register_address(integer_to_list(Address), ReminderGroup);
register_address(Address, ReminderGroup = #reminder_group{project_id = ProjectId, addresses = AddrYaml}) -> 
  case ReminderGroup:has_address(Address) of
    true -> ReminderGroup;
    false ->
      % store contact in phonebook
      contact:find_or_create_with_address_as_anonymous(ProjectId, Address),

      Addresses = active_record_yaml:deserialize(AddrYaml),
      NewAddresses = lists:append(Addresses, [Address]),
      ReminderGroup#reminder_group{addresses = active_record_yaml:serialize(NewAddresses)}
  end.

deregister_address(AddressBin, ReminderGroup = #reminder_group{addresses = AddrYaml}) ->
  case ReminderGroup:has_address(AddressBin) of
    true ->
      Addresses = active_record_yaml:deserialize(AddrYaml),
      ReminderGroup#reminder_group{addresses = active_record_yaml:serialize(address:remove(AddressBin, Addresses))};
    false ->
      ReminderGroup
  end.