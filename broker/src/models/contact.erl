-module(contact).
-export ([find_or_create_with_address/2, find_or_create_with_address_as_anonymous/2]).
-define(TABLE_NAME, "contacts").
-include_lib("erl_dbmodel/include/model.hrl").

find_or_create_with_address(ProjectId, Address) ->
  find_or_create_with_address(ProjectId, Address, undefined).

find_or_create_with_address_as_anonymous(ProjectId, Address) ->
  find_or_create_with_address(ProjectId, Address, 1).

find_or_create_with_address(ProjectId, Address, Anonymous) ->
  case contact_address:find([{project_id, ProjectId}, {address, Address}]) of
    undefined ->
      Contact = contact:create(#contact{project_id = ProjectId, anonymous = Anonymous}),
      contact_address:create(#contact_address{project_id = ProjectId, contact_id = Contact#contact.id, address = Address}),
      Contact;
    #contact_address{contact_id = ContactId} ->
      contact:find(ContactId)
  end.