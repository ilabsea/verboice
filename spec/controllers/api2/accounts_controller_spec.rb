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

describe Api2::AccountsController do
  include Devise::TestHelpers

  let(:admin) { Account.make role: Account::ADMIN }
  let(:user) { Account.make role: Account::USER }

  describe "GET index" do
    context "admin user" do
      context 'host is not allowed' do
        before(:each) do
          request.stub(:remote_ip).and_return('192.192.192.192')
        end

        it 'response with 401' do
          get :index, email: admin.email, token: admin.auth_token

          assert_response :unauthorized
        end
      end

      context 'host is allowed' do
        before(:each) do
          Account.make role: Account::USER
        end

        it "response with 200" do
          get :index, email: admin.email, token: admin.auth_token

          assert_response :success
        end

        it "response json array of normal user accounts" do
          get :index, email: admin.email, token: admin.auth_token

          accounts = ActiveSupport::JSON.decode(@response.body)
          accounts.length.should eq(1)
        end
      end
    end

    context "normal user" do
      it "response with 401" do
        get :index, email: user.email, token: user.auth_token
        
        assert_response :unauthorized
      end
    end
  end

end