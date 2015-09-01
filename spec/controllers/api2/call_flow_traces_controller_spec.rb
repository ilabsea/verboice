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

describe Api2::CallFlowTracesController do

  let(:user) { Account.make role: Account::USER }
  let(:project) { user.projects.make }
  let(:call_flow) { project.call_flows.make }

  describe "index" do
    context "require parameters" do
      it "response 422 when call flow is missing" do
        get :index, email: user.email, token: user.auth_token

        assert_response :unprocessable_entity
      end
    end

    context "invalid date format" do
      context "start date" do
        it "response 422 when start date is invalid" do
          get :index, email: user.email, token: user.auth_token, start_date: '0505/2014'

          assert_response :unprocessable_entity
        end

        it "response 422 when start date is invalid" do
          get :index, email: user.email, token: user.auth_token, start_date: '0505/2014', end_date: '05/05/2014'

          assert_response :unprocessable_entity
        end
      end

      context "end date" do
        it "response 422 when end date is invalid" do
          get :index, email: user.email, token: user.auth_token, end_date: '0505/2014'

          assert_response :unprocessable_entity
        end

        it "response 422 when end date is invalid" do
          get :index, email: user.email, token: user.auth_token, start_date: '05/05/2014', end_date: '0505/2014'

          assert_response :unprocessable_entity
        end
      end
    end

    it "response 200" do
      get :index, email: user.email, token: user.auth_token, call_flow_id: call_flow.id

      assert_response :ok
    end

  end
end
