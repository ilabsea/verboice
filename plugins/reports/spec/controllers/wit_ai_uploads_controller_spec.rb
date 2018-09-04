require 'spec_helper'
describe WitAiUploadsController, :type => :controller  do
  include Devise::TestHelpers
  let(:account) { Account.make }
  let(:project) { Project.make :account => account }
  let(:call_flow) { CallFlow.make :project => project }
  let(:call_log) {CallLog.make :project => project, :call_flow => call_flow}
  let(:channel) { Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :account => account }
  let(:file_like_object) { double("file like object") }


  let(:file) { Rack::Test::UploadedFile.new(Rails.root.join('spec', 'fixtures', 'data.xls'), 'application/vnd.ms-excel') }

  
  before(:each) do
    sign_in account
    wit_ai_body = {"_text"=>"There are two cases of headaches and eye problems and one case of wheezing in Hanoi", "entities"=>{"case"=>[{"suggested"=>true, "entities"=>{"number"=>[{"confidence"=>1, "value"=>2, "type"=>"value"}], "symptom"=>[{"confidence"=>0.99253820506672, "value"=>"Headaches", "type"=>"value"}, {"confidence"=>0.98840411749372, "value"=>"Eye problems", "type"=>"value"}]}, "confidence"=>0.98351878707306, "value"=>"two cases of headaches and eye problems", "type"=>"value"}, {"suggested"=>true, "entities"=>{"number"=>[{"confidence"=>1, "value"=>1, "type"=>"value"}], "symptom"=>[{"confidence"=>0.99765595352748, "value"=>"Wheezing", "type"=>"value"}]}, "confidence"=>0.99002566105536, "value"=>"one case of wheezing", "type"=>"value"}], "location"=>[{"confidence"=>0.93684, "value"=>"Hanoi", "resolved"=>{"values"=>[{"name"=>"Hanoi", "grain"=>"locality", "type"=>"resolved", "timezone"=>"Asia/Ho_Chi_Minh", "coords"=>{"lat"=>21.024499893188, "long"=>105.84117126465}, "external"=>{"geonames"=>"1581130", "wikipedia"=>"Hanoi"}}, {"name"=>"Hanoi", "grain"=>"region", "type"=>"resolved", "timezone"=>"Asia/Ho_Chi_Minh", "coords"=>{"lat"=>21, "long"=>105.75}, "external"=>{"geonames"=>"1581129"}}]}}]}, "msg_id"=>"0MxyMIh3lDBsJT0aQ"}
    stub_request(:post, "https://speech.googleapis.com/v1/speech:recognize?key=AIzaSyCH6lu4WD2i4NXNbOPD8n6exAZYx0e6G8U").
  with(:body => "{\"config\":{\"encoding\":\"linear16\",\"sampleRateHertz\":8000,\"languageCode\":\"vi-VN\",\"maxAlternatives\":3},\"audio\":{\"content\":null}}",
       :headers => {'Accept'=>'application/json', 'Accept-Encoding'=>'gzip, deflate', 'Content-Length'=>'125', 'Content-Type'=>'application/json', 'User-Agent'=>'Ruby'}).
  to_return(:status => 200, :body => "There are two cases of headaches and eye problems and one case of wheezing in Hanoi", :headers => {})
    stub_request(:get, "https://api.wit.ai/message?q=There%20are%20two%20cases%20of%20headaches%20and%20eye%20problems%20and%20one%20case%20of%20wheezing%20in%20Hanoi").
         with(:body => "{}",
              :headers => {'Accept'=>'application/vnd.wit.20160516+json', 'Authorization'=>'Bearer BWPFAQN6QT3ITB5ZSJNN35BCD2Z6BU7B', 'Content-Type'=>'application/json', 'User-Agent'=>'Ruby'}).
         to_return(:status => 200, :body => wit_ai_body.to_json, :headers => {})
  end


  context 'when process upload' do

    it "should read file param and write to a new place for import" do
      post :upload_excel, :project_id => project.id, :file => file 
      file_name = Rails.root.join('public', 'uploads', file.original_filename)
      # File.should_receive(:open).with(file_name, "wb")
      # file.should_receive(:read).with("text")
      File.exist?(file_name).should eq(true)
    end

  end

end
