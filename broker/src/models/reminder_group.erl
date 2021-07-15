-module(reminder_group).
-export([has_address/2, register_address/2, deregister_address/2]).
-define(TABLE_NAME, "ext_reminder_groups").

-include_lib("erl_dbmodel/include/model.hrl").

has_address(Address, ReminderGroup) when is_binary(Address) -> has_address(binary_to_list(Address), ReminderGroup);
has_address(Address, ReminderGroup) when is_integer(Address) -> has_address(integer_to_list(Address), ReminderGroup);
has_address(Address, #reminder_group{id = Id}) -> 
  case reminder_group_contact:find([{reminder_group_id, Id}, {address, Address}]) of
    undefined -> false;
    _ -> true
  end.

register_address(Address, ReminderGroup) when is_binary(Address) -> register_address(binary_to_list(Address), ReminderGroup);
register_address(Address, ReminderGroup) when is_integer(Address) -> register_address(integer_to_list(Address), ReminderGroup);
register_address(Address, #reminder_group{id = Id, project_id = ProjectId}) -> 
  GroupContact = reminder_group_contact:find([{reminder_group_id, Id}, {address, Address}]),
  case GroupContact of
    undefined ->
      % store contact in phonebook
      contact:find_or_create_with_address_as_anonymous(ProjectId, Address),

      reminder_group_contact:create(#reminder_group_contact{reminder_group_id = Id, address = Address});
    _ -> GroupContact
  end.

deregister_address(Address, ReminderGroup) when is_binary(Address) -> deregister_address(binary_to_list(Address), ReminderGroup);
deregister_address(Address, ReminderGroup) when is_integer(Address) -> deregister_address(integer_to_list(Address), ReminderGroup);
deregister_address(Address, #reminder_group{id = Id}) ->
  GroupContact = reminder_group_contact:find([{reminder_group_id, Id}, {address, Address}]),
  case GroupContact of
    undefined -> ok;
    _ -> GroupContact:delete()
  end.
