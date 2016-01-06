-module(address).

-export([exist/2, remove/2]).

exist(Address, Addresses) ->
  case lookup(Address, Addresses) of
    undefined -> false;
    _ -> true
  end.

remove(Address, Addresses) ->
  ExistingAddress = lookup(Address, Addresses),
  lists:delete(ExistingAddress, Addresses).

%% @private
lookup(_Address, []) -> undefined;
lookup(Address, [ExistingAddress | Rest]) ->
  case tel:without_prefix(ExistingAddress) == tel:without_prefix(Address) of
    true -> ExistingAddress;
    _ -> lookup(Address, Rest)
  end.
