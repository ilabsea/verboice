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

describe Ext::Services::PregnanciesController
#   include Devise::TestHelpers

#   let!(:account) { Account.make }

#   before(:each) do
#     sign_in account
#     @project = Project.create name: "foo", account: account
#     @call_flow = CallFlow.make project: @project
#     @call_log = CallLog.create project_id: @project.id, account: account, channel: Channels::Custom.make, call_flow: @call_flow
#   end
  
#   # let!(:project) { Project.make account: account }
#   # let!(:call_flow) { CallFlow.make project: project }
#   # let!(:call_log) { CallLog.make project: project }

#   context "Reminder registration" do
#     it "should ignore when caller has been in reminder phone book listed" do
#       Ext::ReminderPhoneBook.create name: "reminder schedule", project: @project, phone_number: "85569860012"
#       post :register, :status => "1", :CallSid => @call_log.id, :From => "85569860012"
#       Ext::ReminderPhoneBook.count.should eq(1)
#     end

#     it 'should subscribe caller into reminder phone book to receive reminder schedule' do
#       Ext::ReminderPhoneBook.count.should eq(0)
#       post :register, :status => "1", :CallSid => @call_log.id, :From => "85569860012"
#       Ext::ReminderPhoneBook.count.should eq(1)
#     end

#     it 'should unsubscribe caller from phone book to stop receive reminder schedule' do
#       Ext::ReminderPhoneBook.create name: "reminder schedule", project: @project, phone_number: "85569860012"
#       post :register, :status => "2", :CallSid => @call_log.id, :From => "85569860012"
#       Ext::ReminderPhoneBook.count.should eq(0)
#     end
#   end

#   context "Pregnancy progress" do
#     before(:each) do
#       @reminder_phone_book = Ext::ReminderPhoneBook.create name: "reminder schedule", project: @project, phone_number: "85569860012"
#       @patient = Ext::Patient.create reminder_phone_book_id: @reminder_phone_book.id
#     end

#     it "should store the pregnancy date as 1 week ago" do
#       post :progress, :type => "1", :duration => "1", :From => "85569860012", :CallSid => @call_log.id
#       @patient.reload.pregnancy_date.should eq(7.days.ago.to_date)
#     end

#     it "should store the pregnancy date as 1 month ago" do
#       post :progress, :type => "2", :duration => "1", :From => "85569860012", :CallSid => @call_log.id
#       @patient.reload.pregnancy_date.should eq(30.days.ago.to_date)
#     end
#   end
# end
