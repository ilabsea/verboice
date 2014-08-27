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

  describe "create" do
    context "sign in as admin" do
      context 'host is not allowed' do
        before(:each) do
          request.stub(:remote_ip).and_return('192.168.1.1')
        end

        it "response with 401" do
          post :create, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: 9999, enabled: true, blocked: false}

          assert_response :unauthorized
        end
      end

      context 'host is allowed' do
        before(:each) do
          request.stub(:remote_ip).and_return('127.0.0.1')
        end

        context "channel doesn't exists" do
          it "response with 403" do
            post :create, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: 9999, enabled: true, blocked: false}

            assert_response :bad_request
          end
        end

        context 'channel is exists' do
          before(:each) do
            @channel_account = Channels::Custom.make account: account
            @channel_admin = Channels::Custom.make account: admin
          end

          context "with channel quota params" do
            it "response with 200" do
              @channel_account.quota.should be_nil

              post :create, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: @channel_account.id, enabled: true, blocked: false}

              @channel_account.reload.quota.should_not be_nil
              @channel_account.quota.enabled.should eq(true)

              assert_response :ok
            end
          end

          context 'without channel_quota params' do
            it "response with 200" do
              @channel_account.quota.should be_nil

              post :create, email: admin.email, token: admin.auth_token, channel_id: @channel_account.id, enabled: true, blocked: false

              @channel_account.reload.quota.should_not be_nil
              @channel_account.reload.quota.enabled.should eq(true)

              assert_response :ok
            end
          end

          context 'update existing quota' do
            it "response with 200" do
              @channel_account.create_quota()
              @channel_account.quota.blocked.should eq(false)

              post :create, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: @channel_account.id, enabled: true, blocked: true}

              @channel_account.reload.quota.blocked.should eq(true)

              assert_response :ok
            end
          end
        end
      end
    end

    context "sign in as normal user" do
      it "unauthorized when it's normal user" do
        post :create, email: account.email, token: account.auth_token

        assert_response :unauthorized
      end
    end
  end

  describe "destroy" do
    context "sign in as normal user" do
      it "response with 401" do
        post :create, email: account.email, token: account.auth_token

        assert_response :unauthorized
      end
    end

    context "sign in as admin" do
      context "host is not allowed" do
        before(:each) do
          request.stub(:remote_ip).and_return('192.168.1.1')
        end

        it "response with 401" do
          delete :destroy, email: admin.email, token: admin.auth_token, channel_quota: {channel_id: 9999, enabled: true, blocked: false}

          assert_response :unauthorized
        end
      end
      context "host is allowed" do
        before(:each) do
          request.stub(:remote_ip).and_return('127.0.0.1')
        end

        context "quota not found" do
          it "response with 403" do
            delete :destroy, email: admin.email, token: admin.auth_token, id: 9999

            assert_response :bad_request
          end
        end

        context "quota found" do
          let(:channel_account) { Channels::Custom.make account: account }
          let(:quota) { channel_account.create_quota }
          it "response with 200" do
            delete :destroy, email: admin.email, token: admin.auth_token, id: quota.id

            assert_response :ok
          end
        end
      end
    end
  end
end
