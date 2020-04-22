require 'spec_helper'

describe ChannelsController do
  include Devise::TestHelpers

  context 'admin' do
    let(:account) { Account.make(role: Account::ADMIN) }
    let(:project) { account.projects.make }
    let!(:call_flow) { CallFlow.make project: project }

    let(:other_project) { Project.make }

    before(:each) do
      sign_in account
    end

    it "shows call flows in new form" do
      get :new, type: "Channels::Custom", template: "Custom"
      assigns(:channel).should be_a_new(Channel)
      assigns(:projects).should eq([project])
    end

    it "load shared projects in new form" do
      Permission.create!(account_id: account.id, type: "Project", model_id: other_project.id, role: :admin)
      get :new, type: "Channels::Custom", template: "Custom"
      assigns(:channel).should be_a_new(Channel)
      assigns(:projects).should eq([project, other_project])
    end
  end
  context 'user' do
    let(:account) { Account.make }
    before(:each) do
      sign_in account
    end

    it 'returns head 403' do
      get :new, type: "Channels::Custom", template: "Custom"
      response.status.should eq(403)
    end
  end
end
