require 'spec_helper'

describe Ext::Parser::DateTimeParser  do
  describe ".parse" do
    it "should parse date time correctly with the format %Y-%m-%d %H:%M" do
      [ 
        {date_time_string: "2012-12-23 10:20", timezone: "Bangkok", format: Ext::ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, result: "2012-12-23T10:20:00+07:00"},
        {date_time_string: "2012-10-21 9:20", timezone: "Cairo", format: Ext::ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, result: "2012-10-21T09:20:00+02:00"}
      ].each do |elm|
        date_time = Ext::Parser::DateTimeParser.parse(elm[:date_time_string], elm[:format], elm[:timezone])
        date_time.class.should eq DateTime
        date_time.to_s.should eq elm[:result]
      end
    end

    it "should raise exception with other format  " do
      [{date_time_string: "18/02/2012 13:20", format: Ext::ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, timezone: "Bangkok", result: "18/02/2012 13:20"}].each do |elm|
        lambda {
          Ext::Parser::DateTimeParser.parse(elm[:date_time_string], elm[:format], elm[:timezone]).class.should
        }.should raise_error ArgumentError
      end
    end
  end
end