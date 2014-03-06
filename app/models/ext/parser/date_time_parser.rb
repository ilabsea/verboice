module Ext
  module Parser
    class DateTimeParser
      def self.parse date_time_string, format = DateTime::DEFAULT_FORMAT_WITHOUT_TIMEZONE, timezone="UTC"
        date_time = DateTime.strptime date_time_string, format
        zone = ActiveSupport::TimeZone.new(timezone)
        DateTime.new(date_time.year, date_time.month, date_time.day, date_time.hour, date_time.min, date_time.sec, "+#{zone.utc_offset/(60*60)}")
      end
    end
  end
end