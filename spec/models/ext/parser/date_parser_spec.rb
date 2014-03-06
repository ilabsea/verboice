require 'spec_helper'

describe Ext::Parser::DateParser do
  describe "DateParser.parse" do
    it "should parse date correctly with the format %Y-%m-%d" do
      date_string = '10/01/2013'
      date = Ext::Parser::DateParser.parse(date_string)

      date.class.should eq Date
      date.year.should eq 2013
      date.month.should eq 1
      date.day.should eq 10
    end
  end
end