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

  before(:each) do
    sign_in account
  end

  describe "list" do
    before(:each) do
      CallFlow.make project: project
    end

    it "should list all call flows" do
      get :list

      assert_response :ok
      response = ActiveSupport::JSON.decode(@response.body)
      response.length.should eq(1)
    end
  end

end