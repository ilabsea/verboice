-module(tel).

-export([without_prefix/1, possible_addresses/1]).

-define(PREFIXES, [
  "855",
  "+855",
  "0",
  "+0",
  "+"
]).

without_prefix(Address) when is_binary(Address) -> without_prefix(binary_to_list(Address));
without_prefix(Address) -> without_prefix(Address, ?PREFIXES).

%% @private
without_prefix(Address, []) -> Address;
without_prefix(Address, [Prefix | Rest]) ->
  Length = string:len(Prefix),
  AddressPrefix = string:substr(Address, 1, Length),

  if 
    Prefix /= AddressPrefix -> 
      without_prefix(Address, Rest);
    true ->
      string:substr(Address, Length + 1)
  end.

possible_addresses(Address) ->
  possible_addresses(without_prefix(Address), ?PREFIXES).

possible_addresses(AddressWithoutPrefix, []) -> [AddressWithoutPrefix];
possible_addresses(AddressWithoutPrefix, [Prefix | Rest]) ->
  [Prefix ++ AddressWithoutPrefix | possible_addresses(AddressWithoutPrefix, Rest)].