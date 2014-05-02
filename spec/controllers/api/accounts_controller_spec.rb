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

describe Api::AccountsController do
  include Devise::TestHelpers

  let(:admin) { Account.make role: Account::ADMIN }
  let(:user) { Account.make role: Account::USER }

  describe "GET index" do
    context "admin" do
      before(:each) do
        Account.make role: Account::USER

        sign_in admin
      end

      it "should response json array of accounts" do
        Billing.should_receive(:configured?).and_return(true)
        Billing.should_receive(:api_key).and_return("VALID_KEY")

        get :index, api_key: "VALID_KEY"

        assert_response :success
        accounts = ActiveSupport::JSON.decode(@response.body)
        accounts.length.should eq(2)
      end
    end

    context "user" do
      before(:each) do
        sign_in user
      end

      it "should not found" do
        get :index, api_key: "VALID_KEY"
          
        assert_response :not_found
      end
    end
  end

end