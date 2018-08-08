require 'net/http'
class WitAisController < ApplicationController
  # before_filter :authenticate_account!

  def call_finished
    params = {"CallSid"=>"391", "CallStatus"=>"completed", "From"=>"verboice_13", "CallDuration"=>"10", "action"=>"call_finished", "controller"=>"wit_ais"}
    finished_calls = ['completed', 'failed']
    return unless finished_calls.include?(params['CallStatus'])
    speech = Speech.new(Options.google_api_key)
    audio_file = "1533197539228"
    audio_url = "#{Options.verboice_url}/calls/#{params['CallSid']}/results/#{audio_file}"
    begin
      log = CallLog.find params['CallSid']
      file_audio = RecordingManager.for(log).result_path_for(audio_file)
      result = speech.recognize file_audio, Options.language_recognition
      message = result['transcript']
      message = "There are two cases of malaria, one case of fever in HalongBay"
      wit = Wit.new(access_token: Options.wit_token)
      json_response = wit.message message
    rescue Exception => e
      puts e.message
    end

    render json: json_response.merge('caller' => params['From'], 'audio_url' => audio_url, 'reported_at' => Time.now)
  end

end