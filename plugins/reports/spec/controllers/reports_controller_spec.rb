require 'spec_helper'

describe ReportsController, :type => :controller  do
	include Devise::TestHelpers
  include AuthHelper

  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make :project => project }
  let(:call_log) {CallLog.make :project => project, :call_flow => call_flow}
  
  before(:each) do
    http_login account
  end

  context 'should create report after call finished' do
    let(:params) {
      {
        'CallSid' => call_log.id, 
        'CallStatus' => 'Missing', 
        'From' => 'verboice_13', 
        'CallDuration' => '10'
      }
    }

    it 'response empty string' do
      get :call_finished, params

      response.body.should eq('')
    end
  end

  context 'should create report after call finished' do
    before(:each) do
      CallManager.stub(:handle_call_finished).with(call_log.id.to_s).and_yield({})
      Parsers::ReportParser.stub(:parse).with({}).and_return(Report.new)
    end

    let(:params) {
      {
        'CallSid' => call_log.id, 
        'CallStatus' => 'completed', 
        'From' => 'verboice_13', 
        'CallDuration' => '10'
      }
    }

    it 'create a new report' do
      expect {
        get :call_finished, params
      }.to change { Report.count }.by 1
    end
  end

end
