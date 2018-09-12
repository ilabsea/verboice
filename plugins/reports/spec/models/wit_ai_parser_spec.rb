require 'spec_helper'

describe WitAiParser, :type => :model do

  before(:each) do 
    
  end

  it "should collect symptom and number of case in case object " do
    parser = WitAiParser.new(9)
    response_obj = {
                      "_text"=>"Paris his cases of ice problems and headaches and one piece of wheezing in Hanoi.", 
                      "entities"=>{
                        Reports::Settings.number_of_case_key=>[{
                          "confidence"=>0.93002406626739, 
                          "value"=>"20", "type"=>"value"
                        }], 
                        Reports::Settings.symptoms_key => [{
                          "confidence"=>0.93002406626739, 
                          "value"=>"headaches", 
                          "type"=>"value" 
                        },
                        {
                          "confidence"=>0.93002406626739, 
                          "value"=>"wheezing", 
                          "type"=>"value"
                        }], 
                        Reports::Settings.location_key=>[{
                          "confidence"=>0.76425190431811, 
                          "value"=>"Hanoi", 
                          "type"=>"value"
                        }]
                      },
                      "msg_id"=>"1YAfGo0sIX35x957L"
                  }
    report = parser.parse_to_report(response_obj)
    report.message.should eq("Paris his cases of ice problems and headaches and one piece of wheezing in Hanoi.")
    report.location.should eq("Hanoi")
    report.properties.should eq({0=>{Reports::Settings.symptoms_key=>["headaches", "wheezing"], Reports::Settings.number_of_case_key => "20"}})
    report.call_id.should eq(9)
  end

  it "should collect symptom and number of case even they are out of case object " do
    response_obj = {"_text"=>"Paris his cases of ice problems and headaches and one piece of wheezing in Hanoi.", "entities"=>{"symptom"=>[{"confidence"=>0.93002406626739, "value"=>"headaches", "type"=>"value"}], "place"=>[{"confidence"=>0.76425190431811, "value"=>"Hanoi", "type"=>"value"}]}, "msg_id"=>"1YAfGo0sIX35x957L"}
    parser = WitAiParser.new(9)
    properties = parser.extract_properties_from_none_case(response_obj)
    properties.should eq({"symptom"=>["headaches"]})
  end


end