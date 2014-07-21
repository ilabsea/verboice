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

describe Api2::CallLogsController do

  let(:admin) { Account.make role: Account::ADMIN}
  let(:account) { Account.make role: Account::USER }
  let(:project) { Project.make account: account }
  let(:call_flow) { CallFlow.make project: project }

  before(:each) do
    @call_log = CallLog.make project: project, call_flow: call_flow, address: "012334455", account: account
    @another_call_log = CallLog.make project: project, call_flow: call_flow, address: "012778899", account: account
  end

  describe "index" do
    before(:each) do
      CallLog.make account: admin
    end

    context "admin" do
      context "host is not allowed" do
        it "unauthorized" do
          request.stub(:remote_ip).and_return('192.168.1.1')
          get :index, email: admin.email, token: admin.auth_token

          assert_response :unauthorized
        end
      end

      context "host is allowed" do
        it "list all call logs when host is allowed" do
          get :index, email: admin.email, token: admin.auth_token

          assert_response :ok
          response = JSON.parse(@response.body)
          p "---------"
          p response

          expect(response.length).to eq 3
        end

        it "list only call logs that belongs to the account" do
          get :index, email: admin.email, token: admin.auth_token, account_id: account.id

          assert_response :ok
          response = JSON.parse(@response.body)
          expect(response.length).to eq 2
        end
      end
    end

    context "normal user" do
      it "should response 200" do
        get :index, email: account.email, token: account.auth_token

        assert_response :ok
      end

      it "list all call logs" do
        get :index, email: account.email, token: account.auth_token

        response = ActiveSupport::JSON.decode(@response.body)
        response.length.should == 2
      end

      context "by address" do
        context "it is not exists" do
          it "response 200" do
            get :index, email: account.email, token: account.auth_token, address: "012999999"

            assert_response :ok
          end

          it "list of empty call logs" do
            get :index, email: account.email, token: account.auth_token, address: "012999999"

            response = ActiveSupport::JSON.decode(@response.body)
            response.length.should == 0
          end
        end

        context "it is exists" do
          it "response 200" do
            get :index, email: account.email, token: account.auth_token, address: "012334455"

            assert_response :ok
          end

          it "list all those call logs" do
            get :index, email: account.email, token: account.auth_token, address: "012334455"

            response = ActiveSupport::JSON.decode(@response.body)
            response.length.should == 1
          end
        end
      end
    end
  end

  describe "get by channel" do
    before(:each) do
      @channel_user = Channels::Custom.make account: account
      @channel_admin = Channels::Custom.make account: admin

      CallLog.make channel_id: @channel_user.id
      CallLog.make channel_id: @channel_user.id

      CallLog.make channel_id: @channel_admin.id
    end

    it "response 404 when it doesn't exists" do
      get :list_by_channel, email: admin.email, token: admin.auth_token, channel_id: 9999

      assert_response :not_found
    end

    context "super admin" do
      it "response 200 when it is exists" do
        get :list_by_channel, email: admin.email, token: admin.auth_token, channel_id: @channel_admin.id

        assert_response :ok
      end

      it "list all call logs belongs to channel" do
        get :list_by_channel, email: admin.email, token: admin.auth_token, channel_id: @channel_admin.id

        response = ActiveSupport::JSON.decode(@response.body)
        expect(response.length).to eq(1)
      end
    end

    context "normal user" do
      it "response 401 when it is belongs to other user" do
        get :list_by_channel, email: account.email, token: account.auth_token, channel_id: @channel_admin.id

        assert_response :unauthorized
      end

      it "response 200 when it is exists" do
        get :list_by_channel, email: account.email, token: account.auth_token, channel_id: @channel_user.id

        assert_response :ok
      end

      it "list all call logs belongs to channel" do
        get :list_by_channel, email: account.email, token: account.auth_token, channel_id: @channel_user.id

        response = ActiveSupport::JSON.decode(@response.body)
        expect(response.length).to eq(2)
      end
    end
  end

  describe "get by ID" do
    context "when it is not found" do
      it "response 404" do
        get :show, email: account.email, token: account.auth_token, id: 9999
        
        assert_response :not_found
      end
    end

    context "when it is found" do
      it "response 200" do
        get :show, email: account.email, token: account.auth_token, id: @call_log.id
        
        assert_response :ok
      end

      it "show the call log" do
        get :show, email: account.email, token: account.auth_token, id: @call_log.id

        response = ActiveSupport::JSON.decode(@response.body)
        response.should be_kind_of(Hash)
      end
    end
  end

end
