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

describe Api2::ChannelsController do

  let(:admin) { Account.make role: Account::ADMIN }
  let(:account) { Account.make role: Account::USER }
  let(:project) { Project.make account: account }
  let(:call_flow) { CallFlow.make project: project }

  describe "get" do
    it "should return not found for non existing channel" do
      get :get, email: account.email, token: account.auth_token, :name => 'non_existing'
      assert_response :not_found
    end

    it "should return channel" do
      chan = Channels::CustomSip.make account: account, call_flow: call_flow

      get :get, email: account.email, token: account.auth_token, :name => chan.name

      assert_response :ok
      response.body.should eq(chan.to_json)
    end
  end

  describe "create" do
    it "create custom channel" do
      data = {kind: "custom", name: "foo", call_flow: call_flow.name}
      @request.env['RAW_POST_DATA'] = data.to_json
      post :create, email: account.email, token: account.auth_token, format: :json
      assert_response :ok

      channels = account.channels.all
      channels.length.should == 1
      channels[0].account.should == account
      channels[0].call_flow_id.should == call_flow.id
      channels[0].name.should == data[:name]
      channels[0].class.should == Channels::Custom
    end

    it "create a custom sip channel" do
      data = {
        kind: 'sip',
        name: 'foo',
        call_flow: call_flow.name,
        config: {
          username: 'john',
          password: 'secret',
          number: '123',
          limit: '3',
          domain: 'foobar.com',
          direction: 'both',
          register: true
        }
      }

      @request.env['RAW_POST_DATA'] = data.to_json
      post :create, email: account.email, token: account.auth_token, format: :json

      assert_response :ok

      channels = account.channels.all
      channels.length.should == 1
      channels[0].account.should == account
      channels[0].call_flow_id.should == call_flow.id
      channels[0].name.should == data[:name]
      channels[0].username.should == data[:config][:username]
      channels[0].password.should == data[:config][:password]
      channels[0].number.should == data[:config][:number]
      channels[0].limit.should == data[:config][:limit]
      channels[0].domain.should == data[:config][:domain]
      channels[0].direction.should == data[:config][:direction]
      channels[0].register.should == data[:config][:register]
      channels[0].class.should == Channels::CustomSip
    end

    it "create custom channel errors" do
      data = {kind: "custom", call_flow: call_flow.name}
      @request.env['RAW_POST_DATA'] = data.to_json
      post :create, email: account.email, token: account.auth_token, format: :json
      assert_response :ok

      account.channels.count.should == 0

      response = JSON.parse(@response.body).with_indifferent_access
      response[:summary].should == "There were problems creating the Channel"
      response[:properties].should == ["name" => "Can't be blank"]
    end
  end

  describe "update" do
    it "should return not found for non existing channel" do
      put :update, email: account.email, token: account.auth_token, :name => 'non_existing'
      assert_response :not_found
    end

    it "should update channel" do
      chan = Channels::CustomSip.make account: account, call_flow: call_flow

      data = {
        name: 'updated name',
        config: {
          username: 'updated username',
          password: 'updated secret'
        }
      }
      @request.env['RAW_POST_DATA'] = data.to_json
      put :update, email: account.email, token: account.auth_token, name: chan.name, format: :json
      assert_response :ok

      chan = chan.reload
      chan.name.should eq('updated name')
      chan.username.should eq('updated username')
      chan.password.should eq('updated secret')
    end

    it "should tell erros" do
      chan = Channels::Custom.make account: account, call_flow: call_flow, name: 'the_channel'

      data = {:name => ''}
      @request.env['RAW_POST_DATA'] = data.to_json
      put :update, email: account.email, token: account.auth_token, name: chan.name, format: :json

      assert_response :ok

      response = JSON.parse(@response.body).with_indifferent_access
      response[:summary].should == "There were problems updating the Channel"
      response[:properties].should == ["name" => "Can't be blank"]

      chan.reload.name.should eq('the_channel')
    end
  end

  describe "delete" do
    it "should return not found for non existing channel" do
      delete :destroy, email: account.email, token: account.auth_token, :name => 'non_existing'
      assert_response :not_found
    end

    it "delete channel" do
      chan = Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :name => 'foo', :account => account

      delete :destroy, email: account.email, token: account.auth_token, :name => chan.name
      assert_response :ok

      account.channels.count.should == 0
    end
  end

  describe "index" do
    before(:each) do
      Channels::Custom.make account: account
      Channels::Custom.make account: admin
    end

    context "sign in as admin" do
      it "unauthorized when the host is not allowed" do
        request.stub(:remote_ip).and_return('192.168.1.1')
        get :index, email: admin.email, token: admin.auth_token

        assert_response :unauthorized
      end

      it "list all channels when host is allowed" do
        get :index, email: admin.email, token: admin.auth_token

        assert_response :ok
        response = JSON.parse(@response.body)
        expect(response.length).to eq 2
      end

      it "list only channels that belongs to the account" do
        get :index, email: admin.email, token: admin.auth_token, account_id: admin.id

        assert_response :ok
        response = JSON.parse(@response.body)
        expect(response.length).to eq 1
      end
    end

    context "sign in as normal user" do
      it "list all channels those belongs to the account" do
        get :index, email: account.email, token: account.auth_token

        assert_response :ok
        response = JSON.parse(@response.body)
        response.length.should == 1
      end
    end
  end

  describe "mark_as_approved" do
    before(:each) do
      @channel1 = Channels::Custom.make account: account, status: Channel::STATUS_PENDING
      Channels::Custom.make account: admin
    end

    context "sign in as admin" do
      it "unauthorized when the host is not allowed" do
        request.stub(:remote_ip).and_return('192.168.1.1')
        get :mark_as_approved, id: @channel1.id, email: admin.email, token: admin.auth_token

        assert_response :unauthorized
      end

      it "update status of channel to approved" do
        get :mark_as_approved, id: @channel1.id, email: admin.email, token: admin.auth_token, account_id: admin.id

        assert_response :ok
        response = JSON.parse(@response.body)
        expect(response['status']).to eq Channel::STATUS_APPROVED
      end
    end

    context "sign in as normal user" do
      it "list all channels those belongs to the account" do
        get :mark_as_approved, id: @channel1.id, email: account.email, token: account.auth_token

        assert_response :unauthorized
      end
    end
  end

  describe "mark_as_pending" do
    before(:each) do
      @channel1 = Channels::Custom.make account: account, status: Channel::STATUS_APPROVED
      Channels::Custom.make account: admin
    end

    context "sign in as admin" do
      it "unauthorized when the host is not allowed" do
        request.stub(:remote_ip).and_return('192.168.1.1')
        get :mark_as_pending, id: @channel1.id, email: admin.email, token: admin.auth_token

        assert_response :unauthorized
      end

      it "update status of channel to approved" do
        get :mark_as_pending, id: @channel1.id, email: admin.email, token: admin.auth_token, account_id: admin.id

        assert_response :ok
        response = JSON.parse(@response.body)
        expect(response['status']).to eq Channel::STATUS_PENDING
      end
    end

    context "sign in as normal user" do
      it "list all channels those belongs to the account" do
        get :mark_as_pending, id: @channel1.id, email: account.email, token: account.auth_token

        assert_response :unauthorized
      end
    end
  end

end
