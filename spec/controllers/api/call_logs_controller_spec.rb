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

describe Api::CallLogsController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:project) { Project.make account: account }
  let(:call_flow) { CallFlow.make project: project }

  before(:each) do
    @call_log = CallLog.make project: project, call_flow: call_flow, address: "012334455"
    @another_call_log = CallLog.make project: project, call_flow: call_flow, address: "012778899"

    sign_in account
  end

  describe "/index" do
    it "should response 200" do
      get :index

      assert_response :ok
    end

    it "list all call logs" do
      get :index

      response = ActiveSupport::JSON.decode(@response.body)
      response.length.should == 2
    end
  end

  describe "Get by adderss" do
    context "when it is not exists" do
      it "response 200" do
        get :index, address: "012999999"

        assert_response :ok
      end

      it "list of empty call logs" do
        get :index, address: "012999999"

        response = ActiveSupport::JSON.decode(@response.body)
        response.length.should == 0
      end
    end

    context "when it is exists" do
      it "response 200" do
        get :index, address: "012334455"

        assert_response :ok
      end

      it "list all those call logs" do
        get :index, address: "012334455"

        response = ActiveSupport::JSON.decode(@response.body)
        response.length.should == 1
      end
    end
  end

  describe "Get by ID" do
    context "when it is not found" do
      it "response 404" do
        get :show, id: 9999
        
        assert_response :not_found
      end
      
      it "show the error message" do
        get :show, id: 9999
        
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "The call log is not found"
      end
    end

    context "when it is found" do
      it "response 200" do
        get :show, id: @call_log.id
        
        assert_response :ok
      end

      it "show the call log" do
        get :show, id: @call_log.id

        response = ActiveSupport::JSON.decode(@response.body)
        response.should be_kind_of(Hash)
      end
    end
  end

  describe "Destroy collection of calls" do
    context "when from date is missing" do
      it "response 422" do
        delete :destroy_collection, to_date: '2016-01-01'
        
        assert_response :unprocessable_entity
      end
    end

    context "when to date is missing" do
      it "response 422" do
        delete :destroy_collection, from_date: '2016-01-01'
        
        assert_response :unprocessable_entity
      end
    end

    context "response a list of call collection that were destroyed with a specific date range" do
      before(:each) do
        @now = Time.new(2016,1,1, 9,0,0, "+07:00")
        Time.stub(:now).and_return(@now)

        @call_log.created_at = Time.now
        @call_log.save

        @another_call_log.created_at = Time.now + 1.day
        @another_call_log.save
      end

      it "response 200" do
        delete :destroy_collection, from_date: '2016-01-01', to_date: '2016-01-02'
        
        assert_response :ok
      end

      it "destroy every call logs those are match date range" do
        delete :destroy_collection, from_date: '2016-01-01', to_date: '2016-01-02'

        response = ActiveSupport::JSON.decode(@response.body)
        response.should be_kind_of(Array)
        response.size.should eq(1)
      end
    end
  end


end