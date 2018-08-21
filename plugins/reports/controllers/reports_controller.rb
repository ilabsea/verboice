class ReportsController < ApplicationController
  before_filter :authenticate_account!, :except => [:call_finished]

  def index
    @project = Project.find(params[:project_id])
    @reports = Report.all
  end

  def call_finished
    # params = {"CallSid"=>"391", "CallStatus"=>"completed", "From"=>"verboice_13", "CallDuration"=>"10", "action"=>"call_finished", "controller"=>"wit_ais"}
    finished_calls = ['completed', 'failed']
    unless finished_calls.include?(params['CallStatus'])
      render :text => ""
    else
      audio_file = "1533197539228"
      message = Service::from_voice_to_speech(audio_file, params['CallSid'])
      # message = "There are two cases of headaches and eye problems and one case of wheezing in Hanoi"
      json_response = Service::from_speech_to_understanding message
      properties = Service::extract_understanding(json_response["entities"]["case"])
      location = json_response["entities"]["location"][0]["value"]
      Report.create!(:call_id => params['CallSid'], :properties => properties, :location => location)
      render :json => json_response
    end
  end

  def process_excel
    data =  Spreadsheet.open file.path
    for index in 0 .. (data.sheet_count - 1) do
      rows = Service::read_sheet(data,index)
      sheet_name = data.worksheet(index).name
      Service::import_entities(sheet_name, rows)
    end
  end

  def upload_excel
    @index = 0
    @project = Project.find(params[:project_id])
    @file_path = Service::save_csv(params[:file])
    data =  Spreadsheet.open @file_path
    @rows = Service::read_sheet(data,@index)
    @sheet_list = []
    for index in 0 .. (data.sheet_count - 1) do
      @sheet_list << data.worksheet(index).name
    end
  end

  def view_excel
    @index = params[:index].to_i
    @project = Project.find(params[:project_id])
    @file_path = params[:file_path]
    data =  Spreadsheet.open @file_path
    @rows = Service::read_sheet(data,@index)
    @sheet_list = []
    for index in 0 .. (data.sheet_count - 1) do
      @sheet_list << data.worksheet(index).name
    end
    render :upload_excel
  end

end