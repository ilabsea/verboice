-module(boolean).
-export([val/1]).

val(Val) when is_list(Val) ->
  case Val of
    "true" -> true;
    "1" -> true;
    _ -> false
  end;

val(Val) when is_integer(Val) ->
  case Val of
    1 -> true;
    _ -> false
  end;

val(Val) -> Val.
  