# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

describe ApplicationHelper do
  describe "#with_callback_url_fields" do

    shared_examples_for "callback url fields" do |type|
      it "should return the correct field" do
        type = type.to_s << "_" if type

        helper.with_callback_url_fields(type) do |field, field_type|
          field.should be_a(Symbol)
          parsed_field = field.to_s
          parsed_field.should =~ /^#{type}callback/
          parsed_field.should =~ /_(url_user|url_password|url)$/
          field_type_assertion = field =~ /password/ ? :password_field : :text_field
          field_type.should == field_type_assertion
        end
      end
    end

    context "passing ':status'" do
      it_should_behave_like "callback url fields", :status
    end

    context "passing no args" do
      it_should_behave_like "callback url fields"
    end
  end

  describe "#diff_in_second" do
    let(:start_time) { Time.new 2013, 8, 14, 15, 38, 12 }

    it 'should get time different' do
      result = helper.diff_in_second(Time.now, start_time)
      result.should == result.to_i.to_s
    end

    context 'when end_time is nil' do
      it 'should return blank' do
        helper.diff_in_second(nil, start_time).should be_blank
      end
    end
  end

  describe "#datetime_format" do
    let(:date) { Time.utc 2013, 12, 26 }

    it "should date as dd/mm/yy" do
      helper.datetime_format(date, 'UTC').should == "26/12/2013 00:00:00 +0000"
    end

    context "when receive a nil time_zone" do
      let(:local_date) { Time.new 2013, 12, 26, 7, 0, 0, '+07:00' }

      it "should format UTC" do
        helper.datetime_format(local_date, nil).should == "26/12/2013 00:00:00 +0000"
      end
    end
  end
end
