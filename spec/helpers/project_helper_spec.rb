require 'spec_helper'

describe ProjectHelper do
  let(:account) { Account.make }
  let(:project) { Project.make account: account }
  let(:call_flow) { CallFlow.make project: project }

  describe '.call_flows_options' do
    it 'should have "All call flows" option' do
      helper.call_flows_options(project).should include ['All call flows', nil]
    end

    it 'should include existing call flow' do
      element = [call_flow.name, call_flow.id]
      helper.call_flows_options(project).should include element
    end
  end
end