require 'spec_helper'

describe Ext::ReminderGroupsController do
  include Devise::TestHelpers

  let!(:account) { Account.make }

  before(:each) do
    @project = Project.make :account => account
    @reminder_group = Ext::ReminderGroup.make name: "reminder group", project: @project, addresses: []

    sign_in account
  end

  it "should get index" do
    get :index, project_id: @project.id
    response.should be_success
  end

  context "import CSV" do
    it "should redirect to index with alert message when file name is empty" do
      post :import, project_id: @project.id, id: @reminder_group.id, :file_name => ""

      flash[:alert].should eq(I18n.t("controllers.ext.reminder_groups_controller.no_file_found"))
      response.should redirect_to :action => :index
    end

    it "should redirect to index with error message when file extension is not csv" do
      bulk_mp3 = fixture_file_upload('/sample.mp3','audio/mpeg')
      post :import, project_id: @project.id, id: @reminder_group.id, :file_name => bulk_mp3

      flash[:error].should eq(I18n.t("controllers.ext.reminder_groups_controller.invalid_file", :ex => I18n.t("controllers.ext.reminder_groups_controller.invalid_extension")))
      response.should redirect_to :action => :index
    end

    it "should redirect to index with notice message when file extension is csv" do
      bulk_csv = fixture_file_upload('/reminder_group_addresses.csv','text/csv')
      post :import, project_id: @project.id, id: @reminder_group.id, :file_name => bulk_csv

      @reminder_group.reload.addresses.count.should eq(2)
      @reminder_group.reload.addresses.first.should eq("85512345678")
      @reminder_group.reload.addresses.last.should eq("85512876543")

      flash[:notice].should eq(I18n.t("controllers.ext.reminder_groups_controller.successfully_updated", :reminder_group_name => "reminder group"))
      response.should redirect_to :action => :index
    end
  end
end