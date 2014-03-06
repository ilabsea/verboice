module Ext
  module Parser
    class DateParser
      def self.parse date_string, format = Date::DEFAULT_FORMAT
        Date.strptime date_string, format
      end
    end
  end
end