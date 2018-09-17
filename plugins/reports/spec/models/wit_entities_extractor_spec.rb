require 'spec_helper'

describe WitEntitiesExtractor, :type => :model do
  context 'extract entities wit json response' do
    context 'empty entities found in phrase' do
      let(:response) {
        {
          "_text"=>"Parents your case is a buys homes and headaches and one piece of leasing in Enola.", 
          "entities"=>{},
          "msg_id"=>"1VblHLu94CGrq4XqL"
        }
      }

      it "should collect symptom(s) and case in report phrase" do
        entities = WitEntitiesExtractor.extract(response)

        entities['text'].should eq('Parents your case is a buys homes and headaches and one piece of leasing in Enola.')
        entities[Reports::Settings.location_key].should eq('')
        entities[Reports::Settings.report_key].should be_empty
      end
    end

    context 'grouping report phrase' do
      context 'one case report' do
        let(:response) {
          {
            "_text"=>"Parents your case is a buys homes and headaches and one piece of leasing in Enola.", 
            "entities"=>{
              "report"=>[
                {
                  "suggested"=>true, 
                  "entities"=>{
                    "symptom"=>[
                      {
                        "confidence"=>0.98554272541723, 
                        "value"=>"headaches", 
                        "type"=>"value"
                      }
                    ]
                  }, 
                  "confidence"=>0.59770326095625, 
                  "value"=>"Parents your case is a buys homes and headaches", 
                  "type"=>"value"
                }
              ]
            }, 
            "msg_id"=>"1VblHLu94CGrq4XqL"
          }
        }

        it "should collect symptom(s) and case in report phrase" do
          entities = WitEntitiesExtractor.extract(response)

          entities['text'].should eq('Parents your case is a buys homes and headaches and one piece of leasing in Enola.')
          entities[Reports::Settings.report_key].size.should eq(1)
          entities[Reports::Settings.report_key][0].should eq({Reports::Settings.symptoms_key=>['headaches']})
        end
      end

      context 'multi cases report' do
        let(:response) {
          {
            "_text"=>"Parents your case is a buys homes and headaches and one piece of leasing in Enola.", 
            "entities"=>{
              "report"=>[
                {
                  "suggested"=>true, 
                  "entities"=>{
                    "symptom"=>[
                      {
                        "confidence"=>0.98554272541723, 
                        "value"=>"headaches", 
                        "type"=>"value"
                      }
                    ]
                  }, 
                  "confidence"=>0.59770326095625, 
                  "value"=>"Parents your case is a buys homes and headaches", 
                  "type"=>"value"
                }, 
                {
                  "suggested"=>true, 
                  "entities"=>{
                    "case"=>[
                      {
                        "confidence"=>0.93308698006697, 
                        "value"=>"1", 
                        "type"=>"value"
                      }
                    ], 
                    "symptom"=>[
                      {
                        "suggested"=>true, 
                        "confidence"=>0.69500813816761, 
                        "value"=>"leasing", 
                        "type"=>"value"
                      }
                    ]
                  }, 
                  "confidence"=>0.5681134864532, 
                  "value"=>"one piece of leasing", 
                  "type"=>"value"
                }
              ]
            }, 
            "msg_id"=>"1VblHLu94CGrq4XqL"
          }
        }

        it "should collect symptom(s) and case in report phrase" do
          entities = WitEntitiesExtractor.extract(response)

          entities['text'].should eq('Parents your case is a buys homes and headaches and one piece of leasing in Enola.')
          entities[Reports::Settings.report_key].size.should eq(2)
          entities[Reports::Settings.report_key][0].should eq({Reports::Settings.symptoms_key=>['headaches']})
          entities[Reports::Settings.report_key][1].should eq({Reports::Settings.symptoms_key=>['leasing'], Reports::Settings.case_key => "1"})
        end
      end
    end

    context 'none report phrase' do
      let(:response) {
        {
          "_text"=>"Paris his cases of ice problems and headaches and one piece of wheezing in Hanoi.", 
          "entities"=>{
            "symptom"=>[
              {"confidence"=>0.93002406626739, "value"=>"headaches", "type"=>"value"}
            ], 
            "place"=>[
              {"confidence"=>0.76425190431811, "value"=>"Hanoi", "type"=>"value"}
            ]
          }, 
          "msg_id"=>"1YAfGo0sIX35x957L"}
      }

      it "should collect individual symptom and case" do
        entities = WitEntitiesExtractor.extract(response)

        entities['text'].should eq('Paris his cases of ice problems and headaches and one piece of wheezing in Hanoi.')
        entities[Reports::Settings.report_key].size.should eq(1)
        entities[Reports::Settings.report_key][0].should eq({Reports::Settings.symptoms_key=>["headaches"]})
      end
    end

    context 'mixex - grouping report with additional none report phrase' do
      let(:response) {
        {
          "_text"=>"Parents your case is a buys homes and headaches and one piece of leasing in Enola.", 
          "entities"=>{
            "report"=>[
              {
                "suggested"=>true, 
                "entities"=>{
                  "symptom"=>[
                    {
                      "confidence"=>0.98554272541723, "value"=>"headaches", "type"=>"value"
                    }
                  ]
                }, 
                "confidence"=>0.59770326095625, 
                "value"=>"Parents your case is a buys homes and headaches", 
                "type"=>"value"
              }
            ],
            "symptom"=>[
              {
                "confidence"=>0.93002406626739, "value"=>"fever", "type"=>"value"
              }
            ], 
            "place"=>[
              {
                "confidence"=>0.76425190431811, "value"=>"Hanoi", "type"=>"value"
              }
            ]
          },
          
          "msg_id"=>"1VblHLu94CGrq4XqL"
        }
      }

      it "should collect group and individual symptom and case" do
        entities = WitEntitiesExtractor.extract(response)

        entities['text'].should eq('Parents your case is a buys homes and headaches and one piece of leasing in Enola.')
        entities[Reports::Settings.report_key].size.should eq(2)
        entities[Reports::Settings.report_key][0].should eq({Reports::Settings.symptoms_key=>['headaches']})
        entities[Reports::Settings.report_key][1].should eq({Reports::Settings.symptoms_key=>['fever']})
      end
    end
  end
end
