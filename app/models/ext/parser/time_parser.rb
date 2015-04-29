module Ext
  module Parser
    class TimeParser
      def self.parse time_string, format = DateTime::DEFAULT_FORMAT_WITHOUT_TIMEZONE, timezone="UTC"
        time = Time.strptime time_string, format
        zone = ActiveSupport::TimeZone.new(timezone)
        ActiveSupport::TimeWithZone.new(nil, zone, time).to_time
      end
    end
  end
end