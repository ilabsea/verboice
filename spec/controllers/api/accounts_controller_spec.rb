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

      it "should not found when there is no hosts available" do
        Billing.should_receive(:hosts).and_return([])

        request.stub(:remote_ip).and_return('127.0.0.1')

        get :index

        assert_response :not_found
      end

      it "should not found when there is no hosts match" do
        Billing.should_receive(:hosts).and_return([])

        request.stub(:remote_ip).and_return(['192.192.192.192'])

        get :index

        assert_response :not_found
      end

      it "should response json array of accounts" do
        Billing.should_receive(:hosts).and_return(["127.0.0.1"])

        request.stub(:remote_ip).and_return('127.0.0.1')

        get :index

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
        request.stub(:remote_ip).and_return('127.0.0.1')

        get :index
          
        assert_response :not_found
      end
    end
  end

  describe "GET ping" do
    context "when it is admin" do
      before(:each) do
        sign_in admin
      end

      context "and there is no hosts available" do
        it "response with 200" do
          Billing.should_receive(:hosts).and_return([])

          request.stub(:remote_ip).and_return('127.0.0.1')

          get :ping

          assert_response :not_found
        end
      end

      context "and there is no hosts match" do
        it "response with 404" do
          Billing.should_receive(:hosts).and_return(["192.192.192.192"])

          request.stub(:remote_ip).and_return('127.0.0.1')

          get :ping

          assert_response :not_found
        end
      end

      context "and the host is matched" do
        it "response with 404" do
          Billing.should_receive(:hosts).and_return(["127.0.0.1"])

          request.stub(:remote_ip).and_return('127.0.0.1')

          get :ping

          assert_response :ok
        end
      end
    end

    context "when it is user" do
      before(:each) do
        sign_in user
      end

      it "response with 404" do
        request.stub(:remote_ip).and_return('127.0.0.1')

        get :ping

        assert_response :not_found
      end
    end

  end

end