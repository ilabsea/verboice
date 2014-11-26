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

module Commands
  describe DeregisterCommand do

    let(:pbx) { double('pbx') }

    before :each do
      pbx.stub :record

      @project = Project.make
      @contact = Contact.make :project => @project
      @contact_address = ContactAddress.make :address => "1000", :contact => @contact, :project => @project
      @call_flow = CallFlow.make project: @project
      @call_log = CallLog.make call_flow: @call_flow
      @reminder_group_one = @project.ext_reminder_groups.create!(:name => "reminder_one", :addresses => ["1000"])
    end

    it "should deregister caller from reminder group when caller is exists" do
      session = Session.new :pbx => pbx, :call_log => @call_log
      session.stub :address => "1000"

      # before process the step
      @reminder_group_one.reload.addresses.size.should eq(1)

      cmd = DeregisterCommand.new "reminder_one"
      cmd.next = :next
      cmd.run(session).should == :next

      # after process the step
      @reminder_group_one.reload.addresses.size.should eq(0)
    end

    it "should ignore caller deregistration from reminder group when caller is not exists in reminder group" do
      session = Session.new :pbx => pbx, :call_log => @call_log
      session.stub :address => "1001"

      # before process the step
      @reminder_group_one.reload.addresses.size.should eq(1)

      cmd = DeregisterCommand.new "reminder_one"
      cmd.next = :next
      cmd.run(session).should == :next

      @reminder_group_one.reload.addresses.size.should eq(1)
    end

    it "should deregister caller from all reminder group under project" do
      @reminder_group_two = @project.ext_reminder_groups.create!(:name => "reminder_two", :addresses => ["1000"])

      session = Session.new :pbx => pbx, :call_log => @call_log
      session.stub :address => "1000"

      # before process the step
      @reminder_group_one.reload.addresses.size.should eq(1)
      @reminder_group_two.reload.addresses.size.should eq(1)

      cmd = DeregisterCommand.new "All"
      cmd.next = :next
      cmd.run(session).should == :next

      @reminder_group_one.reload.addresses.size.should eq(0)
      @reminder_group_two.reload.addresses.size.should eq(0)
    end

    it "should raise exception when reminder group name is nil" do
      cmd = DeregisterCommand.new nil
      cmd.next = :next
      expect { cmd.run(session).should == :next }.to raise_exception
    end

    it "should raise exception when reminder group doesn't exists" do
      cmd = DeregisterCommand.new "foo"
      cmd.next = :next
      expect { cmd.run(session).should == :next }.to raise_exception
    end

  end
end