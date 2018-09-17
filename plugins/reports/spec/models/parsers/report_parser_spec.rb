require 'spec_helper'

describe Parsers::ReportParser, type: :model do
  context 'parse' do
    let(:json_response) { {} }

    let(:entities) {
      {
        'text' => 'This is testing',
        Reports::Settings.location_key => 'Phnom Penh',
        Reports::Settings.report_key => [
          {
            Reports::Settings.symptoms_key => ['headache'],
            Reports::Settings.case_key => '1',
          }
        ]
      }
    }

    before(:each) do
      WitEntitiesExtractor.stub(:extract).with(json_response).and_return(entities)
    end

    it 'json response to report model' do
      report = Parsers::ReportParser.parse(json_response)

      report.should be_a_kind_of(Report)
      report.message.should eq('This is testing')
      report.properties.should eq([{
        Reports::Settings.symptoms_key => ['headache'],
        Reports::Settings.case_key => '1',
      }])
      report.location.should eq 'Phnom Penh'
    end
  end
end
