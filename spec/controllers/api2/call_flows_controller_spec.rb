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

describe Api2::CallFlowsController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:project) { account.projects.make }

  describe "list" do
    before(:each) do
      @call_flow = CallFlow.make project: project
    end

    it "should response nothing when project ID is missing" do
      get :list, email: account.email, token: account.auth_token

      assert_response :ok
      response = ActiveSupport::JSON.decode(@response.body)
      response.should be_nil
    end

    it "should list all call flows" do
      get :list, email: account.email, token: account.auth_token, project_id: project.id

      assert_response :ok
      response = ActiveSupport::JSON.decode(@response.body)
      response.length.should eq(1)
    end
  end

  describe "show" do
    before(:each) do
      @call_flow = CallFlow.make project: project
    end

    it "should get call flow with specified ID" do
      get :show, email: account.email, token: account.auth_token, id: @call_flow.id

      assert_response :ok
    end
  end

end