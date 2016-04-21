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

describe Api::CallsController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make project: project }
  let(:channel) { Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :account => account }
  let(:schedule) { project.schedules.make }

  before(:each) do
    sign_in account
  end

  context "call" do
    before(:each) do
      BrokerClient.stub(:notify_call_queued)
    end

    it "calls" do
      get :call, :address => 'foo', :channel => channel.name, :callback => 'bar'
      call_log = CallLog.last
      result = JSON.parse(@response.body)
      result['call_id'].should == call_log.id
    end

    it "schedule call in the future" do
      time = Time.now.utc + 1.hour
      get :call, :address => 'foo', :not_before => time, :channel => channel.name
      QueuedCall.first.not_before.time.to_i.should == time.to_i
    end

    it "schedule call in specific schedule" do
      get :call, :address => 'foo', :channel => channel.name, :schedule => schedule.name
      QueuedCall.first.schedule.should == schedule
    end

    it "calls with call flow id" do
      call_flow_2 = CallFlow.make project: project
      get :call, :address => 'foo', :channel => channel.name, :callback => 'bar', :call_flow_id => call_flow_2.id
      CallLog.last.call_flow.should eq(call_flow_2)
    end

    it "calls with call flow name" do
      call_flow_2 = CallFlow.make project: project
      get :call, :address => 'foo', :channel => channel.name, :callback => 'bar', :call_flow => call_flow_2.name
      CallLog.last.call_flow.should eq(call_flow_2)
    end

    it "rejects a call without a channel" do
      get :call
      response.should_not be_success
    end

    it "rejects a call with empty address" do
      get :call, :channel => channel.name
      response.should_not be_success
    end
  end

  it "call state" do
    project = Project.make account: @controller.current_account
    call_log = CallLog.make :call_flow => CallFlow.make(project: project)
    get :state, :id => call_log.id.to_s
    result = JSON.parse(@response.body)
    result['call_id'].should == call_log.id
    result['state'].should == call_log.state.to_s
  end

  context 'bulk_call' do
    before(:each) do
      @reminder_group = Ext::ReminderGroup.make project: project, addresses: ["1000", "2000"]
    end

    it "missing parameter channel_id" do
      post :bulk_call, call_flow_id: 9999
      
      assert_response :unprocessable_entity

      @response.body.should eq("Parameter channel_id is required")
    end

    it "channel ID doesn't exists" do
      post :bulk_call, channel_id: 9999
      
      assert_response :unprocessable_entity

      @response.body.should eq("Channel ID 9999 is not found")
    end

    it "missing parameter call_flow_id" do
      post :bulk_call, channel_id: channel.id
      
      assert_response :unprocessable_entity

      @response.body.should eq("Parameter call_flow_id is required")
    end

    it "call flow ID doesn't exists" do
      post :bulk_call, channel_id: channel.id, call_flow_id: 9999
      
      assert_response :unprocessable_entity

      @response.body.should eq("Call flow ID 9999 is not found")
    end

    it "missing parameter reminder_group_id or addresses to enqueued call" do
      post :bulk_call, channel_id: channel.id, call_flow_id: call_flow.id
      
      assert_response :unprocessable_entity

      @response.body.should eq("Parameter reminder_group_id or addresses is required")
    end

    it "invalid parameter addresses" do
      post :bulk_call, channel_id: channel.id, call_flow_id: call_flow.id, addresses: ""
      
      assert_response :unprocessable_entity
      
      @response.body.should eq("Parameter reminder_group_id or addresses is required")
    end

    it "reminder group ID doesn't exists" do
      post :bulk_call, channel_id: channel.id, call_flow_id: call_flow.id, reminder_group_id: 9999
      
      assert_response :unprocessable_entity

      @response.body.should eq("Reminder group ID 9999 is not found")
    end

    it "Empty addresses to enqueued call" do
      post :bulk_call, channel_id: channel.id, call_flow_id: call_flow.id, addresses: []
      
      assert_response :unprocessable_entity

      @response.body.should eq("Missing addresses(phone numbers) to enqueued call")
    end

    it "enqueued call to every addresses in reminder group" do
      post :bulk_call, channel_id: channel.id, call_flow_id: call_flow.id, reminder_group_id: @reminder_group.id

      assert_response :ok

      response = ActiveSupport::JSON.decode(@response.body)
      
      response.size.should eq(2)

      CallLog.count.should eq(2)
    end

    it "enqueued call to list of phone numbers" do
      post :bulk_call, channel_id: channel.id, call_flow_id: call_flow.id, addresses: [1000, 2000, 3000, 9999]

      assert_response :ok

      response = ActiveSupport::JSON.decode(@response.body)
      
      response.size.should eq(4)
      
      CallLog.count.should eq(4)
    end

    it "ignore duplicate enqueued call to list of phone numbers(numeric)" do
      post :bulk_call, channel_id: channel.id, call_flow_id: call_flow.id, addresses: [1000, 1000, 2000, 2000]

      assert_response :ok

      response = ActiveSupport::JSON.decode(@response.body)
      
      response.size.should eq(2)
      
      CallLog.count.should eq(2)
    end

    it "ignore duplicate enqueued call to list of phone numbers(string)" do
      post :bulk_call, channel_id: channel.id, call_flow_id: call_flow.id, addresses: ['01000', '01000', '2000', '2000']

      assert_response :ok

      response = ActiveSupport::JSON.decode(@response.body)
      
      response.size.should eq(2)
      
      CallLog.count.should eq(2)
    end

  end

end
