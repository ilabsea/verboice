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
    context "admin" do
      before(:each) do
        Account.make role: Account::USER

        sign_in admin
      end

      it "unauthorized when the host is not allowed" do
        request.stub(:remote_ip).and_return('192.168.1.1')
        get :index, email: admin.email, token: admin.auth_token

        assert_response :unauthorized
      end

      it "response json array of accounts when host is allowed" do
        get :index, email: admin.email, token: admin.auth_token

        assert_response :success
        accounts = ActiveSupport::JSON.decode(@response.body)
        accounts.length.should eq(2)
      end
    end

    context "user" do
      before(:each) do
        sign_in user
      end

      it "unauthorized" do
        get :index
          
        assert_response :unauthorized
      end
    end
  end

end