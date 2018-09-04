class ReportsController < ApplicationController
  before_filter :authenticate_account!, :except => [:call_finished]

  def call_finished
    # params = {"CallSid"=>"391", "CallStatus"=>"completed", "From"=>"verboice_13", "CallDuration"=>"10", "action"=>"call_finished", "controller"=>"wit_ais"}
    finished_calls = ['completed', 'failed']
    unless finished_calls.include?(params['CallStatus'])
      render :text => ""
    else
      audio_file = "1533197539228"
      message = Service::from_voice_to_speech(audio_file, params['CallSid'])
      puts message
      # message = "There are two cases of headaches and eye problems and one case of wheezing in Hanoi"
      json_response = Service::from_speech_to_understanding message
      puts json_response
      parser = WitAiParser.new(params['CallSid'])
      parser.parse(json_response)
      report = parser.to_report_object()
      report.save!
      render :json => json_response
    end
  end

end