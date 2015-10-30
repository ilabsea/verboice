-module(contact).
-export([find_or_create_with_address/2, find_or_create_with_address_as_anonymous/2]).
% -export([find_or_create_contact_address/2]).
-export([find_by/2]).

-define(TABLE_NAME, "contacts").
-include_lib("erl_dbmodel/include/model.hrl").

find_by(ProjectId, Address) ->
  PossibleAddresses = tel:possible_addresses(Address), 
  case contact_address:find_all([{project_id, ProjectId}, {address, in, PossibleAddresses}]) of
    [] -> undefined;
    [ContactAddress|_] -> {contact:find(ContactAddress#contact_address.contact_id), ContactAddress}
  end.

find_or_create_with_address(ProjectId, Address) ->
  find_or_create_with_address(ProjectId, Address, undefined).

find_or_create_with_address_as_anonymous(ProjectId, Address) ->
  find_or_create_with_address(ProjectId, Address, 1).

find_or_create_with_address(ProjectId, Address, Anonymous) ->
  case find_by(ProjectId, Address) of
    undefined ->
      Contact = contact:create(#contact{project_id = ProjectId, anonymous = Anonymous}),
      {Contact, create_contact_address(Address, Contact)};
    ContactAndAddress -> ContactAndAddress
  end.

%% @private
create_contact_address(Address, #contact{id = ContactId, project_id = ProjectId}) ->
  contact_address:create(#contact_address{project_id = ProjectId, contact_id = ContactId, address = Address}).