-module(datetime_utils).
-author("Kakada CHHEANG <kakada.chheang@gmail.com>").

-export([in_zone/2, strftime/1, strftime/2]).

-define(DEFAULT_FORMAT, "d/m/Y H:i:s").

in_zone(undefined, DateTime) -> DateTime;
in_zone(TimeZone, {datetime, DateTime}) -> in_zone(TimeZone, DateTime);
in_zone(TimeZone, DateTime) ->
  TimeZoneOffset = case TimeZone of
    undefined -> 0;
    _ -> tz_server:get_timezone_offset(TimeZone)
  end,

  Offset = calendar:datetime_to_gregorian_seconds(DateTime) + TimeZoneOffset,
  calendar:gregorian_seconds_to_datetime(Offset).

strftime({datetime, DateTime}) -> strftime(DateTime);
strftime(DateTime) -> strftime(?DEFAULT_FORMAT, DateTime).

strftime(Format, {datetime, DateTime}) -> strftime(Format, DateTime);
strftime(Format, DateTime) -> dh_date:format(Format, DateTime).
