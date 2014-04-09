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

describe Api::V2::CallLogsController do
  include Devise::TestHelpers

  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make :project => project }
  let(:call_log) {CallLog.make :project => project, :call_flow => call_flow}

  before(:each) do
    sign_in account
  end

  describe "Get show" do
    context "when id is not found" do
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

    context "when id is found" do
      it "response 200" do
        get :show, id: call_log.id
        
        assert_response :ok
      end
    end
  end
end