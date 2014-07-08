-module(reminder_group).
-export([ has_address/2, 
          register_address/2, 
          deregister_address/2,
          address_exist/2,
          address_with_no_prefix/1,
          address_with_no_prefix/2
      ]).


-define(TABLE_NAME, "ext_reminder_groups").

-include_lib("erl_dbmodel/include/model.hrl").

-define(MAP, [
  {addresses, yaml_serializer}
]).

-define(PREFIXES, [
    "855", 
    "+855", 
    "0", 
    "+0", 
    "+"
  ]).


address_exist( _ , []) -> false ;
address_exist(Address, Addresses) ->
  [ExistingAddress|H] = Addresses,
  Result = address_with_no_prefix(ExistingAddress) /= address_with_no_prefix(Address),
  if 
    Result ->
      address_exist(Address, H);
    true ->
      true  
  end.

address_with_no_prefix(Address) ->
  % Prefixes = [ "855", "+855", "0", "+0", "+"],
  address_with_no_prefix(Address, ?PREFIXES).

address_with_no_prefix(Address, []) -> Address;

address_with_no_prefix(Address, Prefixes) ->
  [Prefix|H] =  Prefixes,
  Length = string:len(Prefix),
  AddressPrefix = string:substr(Address, 1, Length),

  if 
    Prefix /= AddressPrefix -> 
      address_with_no_prefix(Address, H);
    true ->  
      string:substr(Address, Length+1)
  end.  

has_address(AddressBin, #reminder_group{addresses = AddrYaml}) ->
  Addresses = active_record_yaml:deserialize(AddrYaml),
  Address = binary_to_list(AddressBin),
  Result = address_exist(Address, Addresses),

  io:format("~n Addresses ~p", [Addresses]),
  io:format("~n Address ~p", [Address]),

  io:format("~n Address: ~p, Addresses: ~p is ~p ~n", [Address, Addresses, Result]),
  %lists:member(Address, Addresses),
  Result.

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