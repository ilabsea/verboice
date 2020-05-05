# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

class CallLogsController < ApplicationController
  before_filter :authenticate_account!
  
  before_filter :paginate, only: [:index, :queued]
  before_filter :search, only: [:index, :download, :download_project_call_logs, :generate_zip]
  before_filter :check_max_row, only: [:download_project_call_logs]
  before_filter :csv_settings, only: [:download, :download_details, :download_project_call_logs]
  before_filter :prepare_log_detail, only: [:show, :progress, :play_result, :download_details]

  helper_method :paginate

  def index
    @logs = @logs.paginate :page => @page, :per_page => @per_page
    render "projects/call_logs/index" if @project
  end

  def show
    set_fixed_width_content
    if params[:project_id].present?
      load_project
      @log = @project.call_logs.find params[:id]
    else
      @log = current_account.call_logs.find params[:id]
    end
    
    @activities = @log.step_activities.sort_by(&:start)
  end

  def progress
    @log = current_account.call_logs.find params[:id]
    render :layout => false
  end

  def queued
    @calls = current_account.queued_calls.includes(:channel).includes(:call_log).includes(:schedule).order('id DESC')
    @calls = @calls.paginate :page => @page, :per_page => @per_page
  end

  def queued_paused
    QueuedCall.pause(params[:queued_call_ids]) if params[:queued_call_ids]
    redirect_to queued_call_logs_path
  end

  def queued_resumed
    QueuedCall.resume(params[:queued_call_ids]) if params[:queued_call_ids]
    redirect_to queued_call_logs_path
  end

  def play_result
    if current_account.projects.find_by_id(@log.project_id) || !current_account.shared_projects.where(:model_id => @log.project_id, :role => 'admin').empty?
      # Checks if the current_user is the owner of @log.project
      # ideally it should use ApplicationController#check_project_admin
      # but it can be done without some further refactors
      send_file RecordingManager.for(@log).result_path_for(params[:key]), :x_sendfile => true
    else
      head :unauthorized
    end
  end

  def download_project_call_logs
    @date_format = params[:date_format]
    render layout: false
  end

  def download_details
    load_project
    @log = @project.call_logs.includes(:entries).find params[:id]
    @activities = @log.step_activities.sort_by(&:start)

    @filename = "Call details #{@log.id} (#{Time.now}).csv"
  end

  def generate_zip
    Delayed::Job.enqueue Jobs::DownloadCallLogsJob.new(current_account.id, @project.id, @search, params[:date_format])
    render layout: false
  end

  def download_zip
    path = File.join RecordingManager.for(current_account).path_for('downloads'), params[:filename]
    send_file path if File.exists? path
  end

  private
    def search
      @search = params[:search] || ""
      if params[:project_id].present?
        %w(phone_number after before call_flow_id).each do |key|
          @search << search_by_key(key)
        end

        load_project
        @logs = @project.call_logs.includes(project: :project_variables).includes(:call_log_answers).includes(:call_log_recorded_audios).order('call_logs.id DESC')
      else
        @logs = current_account.call_logs.includes(:project).includes(:channel).includes(:call_flow).order('call_logs.id DESC')
      end
      @logs = @logs.search @search, :account => current_account if @search.present?
    end
    
    def search_by_key(key)
      params[key].present? ? " #{key}:\"#{params[key]}\"" : ""
    end

    def check_max_row
      if @logs.count > CallLog::CSV_MAX_ROWS
        flash[:error] = I18n.t("controllers.call_logs_controller.csv_is_too_big",
          max: CallLog::CSV_MAX_ROWS, count: @logs.count)
        redirect_to :back
      end
    end

    def csv_settings
      @filename = "Call_logs_(#{Time.now.to_s.gsub(' ', '_')}).csv"
      @output_encoding = 'UTF-8'
      @streaming = true
      @csv_options = { :col_sep => ',' }
    end

    def paginate
      @page = params[:page] || 1
      @per_page = params[:per_page] || 10
    end

    def prepare_log_detail
      @log = CallLog.for_account(current_account).find params[:id]
      @activities = CallLog.poirot_activities(@log.id).sort_by(&:start)
    end
end
