-module(contact).
-export([find_or_create_with_address/2, find_or_create_with_address_as_anonymous/2]).
-export([find_or_create_contact_address/2]).
-define(TABLE_NAME, "contacts").
-include_lib("erl_dbmodel/include/model.hrl").

find_or_create_with_address(ProjectId, Address) ->
  find_or_create_with_address(ProjectId, Address, undefined).

find_or_create_with_address_as_anonymous(ProjectId, Address) ->
  find_or_create_with_address(ProjectId, Address, 1).

find_or_create_with_address(ProjectId, Address, Anonymous) ->
  ContactAddresses = contact_address:find_all([{project_id, ProjectId}]),
  case contact_address(Address, ContactAddresses) of
    undefined ->
      Contact = contact:create(#contact{project_id = ProjectId, anonymous = Anonymous}),
      create_contact_address(Address, Contact),
      Contact;
    ContactAddress -> contact:find(ContactAddress#contact_address.contact_id)
  end.
  
find_or_create_contact_address(Address, Contact = #contact{id = ContactId, project_id = ProjectId}) ->
  ContactAddresses = contact_address:find_all([{project_id, ProjectId}, {contact_id, ContactId}]),
  case contact_address(Address, ContactAddresses) of
    undefined -> create_contact_address(Address, Contact);
    ContactAddress -> ContactAddress
  end.

%% @private
contact_address(_Address, []) -> undefined;
contact_address(Address, [ContactAddress | Rest]) ->
  case tel:without_prefix(ContactAddress#contact_address.address) == tel:without_prefix(Address) of
    true -> ContactAddress;
    _ -> contact_address(Address, Rest)
  end.

%% @private
create_contact_address(Address, #contact{id = ContactId, project_id = ProjectId}) ->
  contact_address:create(#contact_address{project_id = ProjectId, contact_id = ContactId, address = Address}).