class ReportsController < ApplicationController
  before_filter :authenticate_account!, :except => [:call_finished]
  before_filter :authenticate_basic!, :only => [:call_finished]
  before_filter :authenticate_white_list!, :only => [:call_finished]
  def call_finished
    # params = {"CallSid"=>"391", "CallStatus"=>"completed", "From"=>"verboice_13", "CallDuration"=>"10", "action"=>"call_finished", "controller"=>"wit_ais"}
    finished_calls = ['completed', 'failed']
    unless finished_calls.include?(params['CallStatus'])
      render :text => ""
    else
      audio_file = Reports::Settings.verboice_first_audio_file
      message = Service::from_voice_to_speech(audio_file, params['CallSid'])
      # message = "There are two cases of headaches and eye problems and one case of wheezing in Hanoi"
      json_response = Service::from_speech_to_understanding message
      begin
        parser = WitAiParser.new(params['CallSid'])
        report = parser.parse_to_report(json_response)
        report.save!
      rescue
        Report.create!({:message => message, :call_id => params['CallSid'], :properties => {}, :location => nil })
      end
      render :json => json_response
    end
  end

  def authenticate_basic! 
    authenticate_or_request_with_http_basic do |user, password|
      account = Account.find_by_email(user)
      if account.valid_password?(password)
        current_account = account
        return
      end
    end
  end

  def authenticate_white_list!
    head 403 if request.remote_ip != Reports::Settings.verboice_ip
  end

end
