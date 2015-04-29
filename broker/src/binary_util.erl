-module(binary_util).

-export([to_binary/1]).

to_binary(Val) when is_atom(Val) -> atom_to_binary(Val, utf8);
to_binary(Val) when is_list(Val) -> list_to_binary(Val);
to_binary(Val) when is_integer(Val) -> integer_to_binary(Val);
to_binary(Val) when is_float(Val) -> float_to_binary(Val);
to_binary(_) -> throw("Datatype is not supported").