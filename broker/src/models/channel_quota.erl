-module(channel_quota).

-export([enabled/1, blocked/1]).

-define(TABLE_NAME, "channel_quota").
-include_lib("erl_dbmodel/include/model.hrl").

enabled(#channel_quota{enabled = Enabled}) -> Enabled =:= 1.
blocked(#channel_quota{blocked = Blocked}) -> Blocked =:= 1.

