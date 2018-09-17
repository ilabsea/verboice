class ReportsController < ApplicationController
  before_filter :authenticate_account!, :except => [:call_finished]
  before_filter :authenticate_basic!, :only => [:call_finished]
  before_filter :authenticate_whitelist!, :only => [:call_finished]

  # GET /call_finished?CallSid="391", CallStatus="completed", From="verboice_13", CallDuration="10"
  def call_finished
    unless CallLog::FINISHED_CALL_STATUSES.include?(params['CallStatus'])
      render :text => ''
    else
      begin
        CallManager.handle_call_finished(params['CallSid']) do |json_response|
          report = Parsers::ReportParser.parse(json_response)
          
          report.call_id = params['CallSid']
          report.address = params['From']

          report.save!
        end
      rescue
        report = Report.create!({address: params['From'], :message => message, :call_id => params['CallSid'], :properties => [], :location => nil })
      end

      render json: report
    end
  end

  def authenticate_basic!
    authenticate_or_request_with_http_basic do |user, password|
      account = Account.find_by_email(user)
      if account && account.valid_password?(password)
        current_account = account
        return
      end
    end
  end

  def authenticate_whitelist!
    unless Rails.env.test?
      head 403 unless Reports::Settings.whitelist_ips.include?(request.remote_ip)
    end
  end

end
