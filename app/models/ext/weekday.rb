module Ext
  class Weekday
    DAY_NAMES = ["sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"]

    def initialize(wday)
      @wday = wday.to_i
    end

    def symbol
      DAY_NAMES[@wday].to_sym
    end
  end
end