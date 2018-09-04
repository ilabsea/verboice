class WitAiUploadsController < ApplicationController
  before_filter :authenticate_account!, :except => [:call_finished]

  def create
    data =  Spreadsheet.open file.path
    for index in 0 .. (data.sheet_count - 1) do
      rows = Service::read_sheet(data,index)
      sheet_name = data.worksheet(index).name
      Service::import_entities(sheet_name, rows)
    end
  end

  def new
    @index = 0
    @project = Project.find(params[:project_id])
    @file_path = Service::save_excel(params[:file])
    data =  Spreadsheet.open @file_path
    @rows = Service::read_sheet(data,@index)
    @sheet_list = []
    for index in 0 .. (data.sheet_count - 1) do
      @sheet_list << data.worksheet(index).name
    end
  end

  def show
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