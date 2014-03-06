-module(active_record_yaml).

-export([serialize/1]).

-define(INIT_YAML_STR, <<"---\n">>).

to_array(Val) -> 
  ValBin = binary_util:to_binary(Val),
  <<"- ", "'", ValBin/binary, "'\n">>.
to_hash({K, V}) -> 
  KBin = binary_util:to_binary(K),
  VBin = binary_util:to_binary(V),
  <<KBin/binary, ": ", VBin/binary, "\n">>.

serialize([]) -> "---\n";
serialize([H|T]) when is_tuple(H) -> serialize(fun to_hash/1, [H|T], ?INIT_YAML_STR);
serialize(List) -> serialize(fun to_array/1, List, <<"---\n">>).

serialize(_, [], Yaml) -> Yaml;
serialize(Func, [Address | Rest], Yaml ) ->
  AddressBin = Func(Address),
  serialize(Func, Rest, <<Yaml/binary, AddressBin/binary>>).