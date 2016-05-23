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

describe Api::ReminderGroupsController do
  include Devise::TestHelpers

  let!(:account_one) { Account.make }
  let!(:account_two) { Account.make }
  let!(:project) { Project.make account: account_one }
  let!(:another_project) { Project.make account: account_two }
  let!(:reminder_group) { Ext::ReminderGroup.make project: project, addresses: [] }
  let!(:another_reminder_group) { Ext::ReminderGroup.make project: another_project, addresses: [] }

  before(:each) do
    sign_in account_one
  end

  describe "get index" do
    it "should response 404 when project doesn't exists" do
      get :index, project_id: 9999
      
      assert_response :not_found
    end

    it "should response 200" do
      get :index, project_id: project.id

      assert_response :success
    end
  end

  describe "post create" do
    it "should response 404 when project doesn't exists" do
      expect{
        post :create, project_id: 9999
      
        assert_response :not_found
      }.to change(project.ext_reminder_groups, :count).by(0)
    end

    it "should response 400 when parameter is invalid" do
      expect {
        post :create, project_id: project.id
        assert_response :bad_request

        response = JSON.parse(@response.body).with_indifferent_access
        response[:summary].should == "There were problems creating the Ext::ReminderGroup"
        response[:properties].should == [{"name" => "can not be blank"}]
      }.to change(project.ext_reminder_groups, :count).by(0)
    end

    it "should response 400 when addresses passing is not an array" do
      expect {
        post :create, project_id: project.id, reminder_group: {name: "foo", addresses: "1000"}
        assert_response :bad_request

        response = JSON.parse(@response.body).with_indifferent_access
        response[:summary].should == "There were problems creating the Ext::ReminderGroup"
        response[:properties].should == [{"addresses" => "Attribute was supposed to be a Array, but was a String"}]
      }.to change(project.ext_reminder_groups, :count).by(0)
    end

    it "should response 201 with params only name" do
      expect {
        post :create, project_id: project.id, reminder_group: {name: "foo"}

        assert_response :created
      }.to change(project.ext_reminder_groups, :count).by(1)
    end

    it "should response 201" do
      expect {
        post :create, project_id: project.id, reminder_group: {name: "foo", addresses: ["1000"]}

        assert_response :created
      }.to change(project.ext_reminder_groups, :count).by(1)
    end
  end

  describe "put update" do
    it "should response 404 when it doesn't exists" do
      put :update, project_id: project.id, id: 9999

      assert_response :not_found
    end

    it "should response 400 when addresses is string" do
      put :update, project_id: project.id, id: reminder_group.id, reminder_group: { addresses: "1000" }

      assert_response :bad_request
    end

    it "should response 400 when addresses is numeric" do
      put :update, project_id: project.id, id: reminder_group.id, reminder_group: { addresses: 1000 }

      assert_response :bad_request
    end

    it "should response 200 when addresses is empty" do
      put :update, project_id: project.id, id: reminder_group.id, reminder_group: { addresses: [] }

      assert_response :success
      reminder_group.reload.addresses.count.should == 0
  end

    it "should response 200" do
      # expect{
        put :update, project_id: project.id, id: reminder_group.id, reminder_group: { addresses: [1000, 1001, "1000", "1001"] }

        assert_response :success
        reminder_group.reload.addresses.count.should == 2
      # }.to change(reminder_group.addresses, :count).by(2)
    end
  end

  describe "delete destroy" do
    it "should response 404 when it doesn't exists" do
      expect{
        delete :destroy, project_id: project.id, id: 9999

        assert_response :not_found
      }.to change(project.ext_reminder_groups, :count).by(0)
    end

    it "should response 200" do
      expect{
        delete :destroy, project_id: project.id, id: reminder_group.id

        assert_response :success
      }.to change(project.ext_reminder_groups, :count).from(1).to(0)
    end

    
  end

end
