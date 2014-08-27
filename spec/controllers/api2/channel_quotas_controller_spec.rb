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

describe Api2::ChannelQuotasController do

  let(:admin) { Account.make role: Account::ADMIN }
  let(:account) { Account.make role: Account::USER }
  let(:project) { Project.make account: account }
  let(:call_flow) { CallFlow.make project: project }

  describe "create" do
    before(:each) do
      @channel_account = Channels::Custom.make account: account
      @channel_admin = Channels::Custom.make account: admin
    end

    context "sign in as admin" do
      it "unauthorized when the host is not allowed" do
        request.stub(:remote_ip).and_return('192.168.1.1')
        post :create, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: 9999, enabled: true, blocked: false}

        assert_response :unauthorized
      end

      it "unauthorized when channel doesn't exists" do
        request.stub(:remote_ip).and_return('127.0.0.1')
        post :create, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: 9999, enabled: true, blocked: false}

        assert_response :bad_request
      end

      context "create new quota when channel is exists and it doesn't have quota" do
        it "with channel_quota params " do
          @channel_account.quota.should be_nil

          request.stub(:remote_ip).and_return('127.0.0.1')
          post :create, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: @channel_account.id, enabled: true, blocked: false}

          @channel_account.reload.quota.should_not be_nil
          @channel_account.quota.enabled.should eq(true)

          assert_response :ok
        end

        it "without channel_quota params" do
          @channel_account.quota.should be_nil

          request.stub(:remote_ip).and_return('127.0.0.1')
          post :create, email: admin.email, token: admin.auth_token, channel_id: @channel_account.id, enabled: true, blocked: false

          @channel_account.reload.quota.should_not be_nil
          @channel_account.quota.enabled.should eq(true)

          assert_response :ok
        end
      end

      it "update existing quota" do
        @channel_account.create_quota()
        @channel_account.quota.blocked.should eq(false)

        request.stub(:remote_ip).and_return('127.0.0.1')
        post :create, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: @channel_account.id, enabled: true, blocked: true}

        @channel_account.reload.quota.blocked.should eq(true)

        assert_response :ok
      end
    end

    context "sign in as normal user" do
      it "unauthorized when it's normal user" do
        post :create, email: account.email, token: account.auth_token

        assert_response :unauthorized
      end
    end
  end
end
