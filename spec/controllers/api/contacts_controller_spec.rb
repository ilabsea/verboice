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

describe Api::ContactsController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:account_two) { Account.make }
  let!(:project) { Project.make account: account }
  let!(:other_project) { Project.make account: account_two }
  let!(:contact) { project.contacts.make }
  let!(:contact_two) { Contact.make :project => project }
  let!(:other_contact) { Contact.make :project => other_project }

  before(:each) do
    sign_in account

    ContactAddress.make contact_id: contact.id

    @project_var = project.project_variables.make name: "var1"
    @project_var2 = project.project_variables.make name: "var2"
    @contact_var = PersistedVariable.make project_variable_id: @project_var.id, contact_id: contact.id, value: "foo"
  end

  it "gets all contacts" do
    get :index, project_id: project.id

    response.should be_ok

    json = JSON.parse response.body
    json.length.should eq(2)

    json = json[0]
    json['id'].should eq(contact.id)
    json['addresses'].should eq(contact.addresses.map(&:address))
    json['vars'].should eq({"var1" => "foo"})
  end

  it "gets contact by address" do
    get :show_by_address, project_id: project.id, address: contact.addresses.first.address

    response.should be_ok

    json = JSON.parse response.body
    json['id'].should eq(contact.id)
    json['addresses'].should eq(contact.addresses.map(&:address))
    json['vars'].should eq({"var1" => "foo"})
  end

  describe "update a contact's varaible by address" do
    it "response 422 when it's missing parameter" do
      put :update_by_address, project_id: project.id, address: contact.addresses.first.address

      assert_response :unprocessable_entity
    end

    it "update an existing contact variable value" do
      put :update_by_address, project_id: project.id, address: contact.addresses.first.address, vars: {var1: "bar"}

      @contact_var.reload
      @contact_var.value.should eq("bar")

      response.should be_ok

      json = JSON.parse response.body
      json['id'].should eq(contact.id)
      json['addresses'].should eq(contact.addresses.map(&:address))
      json['vars'].should eq({"var1" => "bar"})
    end

    it "update an non-existing contact variable value" do
      put :update_by_address, project_id: project.id, address: contact.addresses.first.address, vars: {var2: "bar"}

      response.should be_ok

      json = JSON.parse response.body
      json['id'].should eq(contact.id)
      json['addresses'].should eq(contact.addresses.map(&:address))
      json['vars'].should eq({"var1" => "foo", "var2" => "bar"})
    end
  end

  it "updates all contacts vars" do
    put :update_all, project_id: project.id, vars: {var1: "bar"}

    @contact_var.reload
    @contact_var.value.should eq("bar")

    json = JSON.parse response.body
    json.length.should eq(2)

    json = json[0]
    json['id'].should eq(contact.id)
    json['addresses'].should eq(contact.addresses.map(&:address))
    json['vars'].should eq({"var1" => "bar"})
  end

  describe "POST create" do
    before(:each) do
      contact.addresses.build(address: "1000").save
    end

    it "should response 404 when project doesn't exists" do
      expect{
        post :create, project_id: 9999

        assert_response :not_found
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "The project is not found"
      }.to change(project.contacts, :count).by(0)
    end

    it "should response 400 when addresses is missing" do
      expect{
        post :create, project_id: project.id

        assert_response :bad_request
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "Addresses is missing"
      }.to change(project.contacts, :count).by(0)
    end

    it "should response 400 when addresses is string" do
      expect {
        post :create, {:project_id => project.id, :addresses => "1000"}

        assert_response :bad_request
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "Addresses was supposed to be a Array, but was a String"
      }.to change(project.contacts, :count).by(0)
    end

    it "should response 400 when addresses is numeric" do
      expect {
        post :create, {:project_id => project.id, :addresses => 1000}

        assert_response :bad_request
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "Addresses was supposed to be a Array, but was a String"
      }.to change(project.contacts, :count).by(0)
    end

    it "should response 200 when addresses is empty" do
      expect {
        post :create, {:project_id => project.id, :addresses => []}

        assert_response :success
      }.to change(project.contacts, :count).by(0)
    end

    it "should ignore existing addresses" do
      expect {
        post :create, {:project_id => project.id, :addresses => ["1000"]}
      }.to change(project.contacts, :count).by(0)
    end

    it "should create non-existing addresses" do
      expect {
        post :create, :project_id => project.id, :addresses => ["01236475", "0243332343", "0186354633"]
      }.to change(project.contacts, :count).by(3)
    end

    it "should create non-existing and ignore existing addresses" do
      expect {
        post :create, :project_id => project.id, :addresses => ["2000", 2000, "3000"]
      }.to change(project.contacts, :count).by(2)
    end
  end

  describe "DELETE unregistration" do
    before(:each) do
      contact.addresses.destroy_all # reset contact addresses to destroy the default of primary address
      contact.addresses.build(address: "1000").save
      contact_two.addresses.destroy_all # reset contact addresses to destroy the default of primary address
      contact_two.addresses.build(address: "2000")
      contact_two.addresses.build(address: "2001")
      contact_two.save
    end

    it "should response 404 when project doesn't exists" do
      expect{
        delete :unregistration, project_id: 9999

        assert_response :not_found
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "The project is not found"
      }.to change(project.contacts, :count).by(0)
    end

    it "should response 400 when addresses is missing" do
      expect{
        delete :unregistration, project_id: project.id

        assert_response :bad_request
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "Addresses is missing"
      }.to change(project.contacts, :count).by(0)
    end

    it "should response 400 when addresses is string" do
      expect {
        delete :unregistration, {:project_id => project.id, :addresses => "1000"}

        assert_response :bad_request
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "Addresses was supposed to be a Array, but was a String"
      }.to change(project.contacts, :count).by(0)
    end

    it "should response 400 when addresses is numeric" do
      expect {
        delete :unregistration, {:project_id => project.id, :addresses => 1000}

        assert_response :bad_request
        response = ActiveSupport::JSON.decode(@response.body)
        response.should == "Addresses was supposed to be a Array, but was a String"
      }.to change(project.contacts, :count).by(0)
    end

    it "should response 200 when addresses is empty" do
      expect {
        delete :unregistration, {:project_id => project.id, :addresses => []}

        assert_response :success
      }.to change(project.contacts, :count).by(0)
    end

    it "should ignore non-existing addresses" do
      expect {
        delete :unregistration, {:project_id => project.id, :addresses => ["9999"]}
      }.to_not change(project.contacts, :count).by(1)
    end

    it "should destroy contact and contact addresses when it has only one address" do
      expect {
        contact.addresses.count.should == 1
        contact.first_address.should == "1000"
        delete :unregistration, {:project_id => project.id, :addresses => ["1000"]}
      }.to change(project.contacts, :count).from(2).to(1)
    end

    it "should destroy only contact adderss when contact has many addresses" do
      expect {
        contact_two.addresses.count.should == 2
        delete :unregistration, {:project_id => project.id, :addresses => ["2000"]}
        contact_two.addresses.count.should == 1
      }.to_not change(project.contacts, :count).by(1)
    end

    it "should destroy existing and ignore non-existing addresses" do
      expect {
        delete :unregistration, {:project_id => project.id, :addresses => ["1000", "9999"]}
      }.to change(project.contacts, :count).from(2).to(1)
    end

    it "should destroy only contact/contact addresses when it's has only one/many addresses" do
      expect {
        delete :unregistration, {:project_id => project.id, :addresses => ["1000", "2000"]}
      }.to change(project.contacts, :count).from(2).to(1)
    end
  end
end
