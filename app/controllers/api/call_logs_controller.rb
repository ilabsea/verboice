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
module Api
  class CallLogsController < ApiController
    before_filter :verify_request, :only => [:show]

    # GET /contacts/:address/call_logs
    # GET /call_logs
    def index
      @call_logs = current_account.call_logs
      
      @call_logs = @call_logs.where(project_id: params[:project_id]) if params[:project_id].present?
      @call_logs = @call_logs.where(call_flow_id: params[:call_flow_id]) if params[:call_flow_id].present?
      
      @call_logs = @call_logs.where("created_at >= ?", params[:from_date]) if params[:from_date].present?
      @call_logs = @call_logs.where("created_at <= ?", params[:to_date]) if params[:to_date].present?
      @call_logs = @call_logs.where("duration >= ?", params[:duration]) if params[:duration].present?
      @call_logs = @call_logs.where(direction: params[:direction]) if params[:direction].present?

      @call_logs = @call_logs.where(state: params[:state]) if params[:state].present?
      @call_logs = @call_logs.where(address: params[:address]) if params[:address].present?

      render json: @call_logs, root: false
    end

    # GET /call_logs/:id
    def show
      render json: @call_log
    end

    # DELETE /api/call_logs
    def destroy_collection
      unless (params[:from_date].present? and params[:to_date].present?)
        render json: "From date and to date parameters are required", status: :unprocessable_entity
        return
      end

      begin
        from_date = Date.strptime(params[:from_date], "%Y-%m-%d")
        to_date = Date.strptime(params[:to_date], "%Y-%m-%d")
      rescue
        render json: "Invalid date format of from date or to date", status: :unprocessable_entity
        return
      end

      if (to_date - 1.month).greater_or_equal? from_date
        render json: "Date range is too big", status: :unprocessable_entity
        return
      end

      call_logs = current_account.call_logs.where("created_at >= ? and created_at <= ?", from_date, to_date)

      call_logs = call_logs.where(project_id: params[:project_id]) if params[:project_id].present?
      call_logs = call_logs.where(call_flow_id: params[:call_flow_id]) if params[:call_flow_id].present?

      affected_call_logs = []

      CallLog.transaction do
        affected_call_logs = call_logs.destroy_all
        QueuedCall.transaction do
          QueuedCall.where(call_log_id: affected_call_logs).destroy_all
        end
      end

      render json: affected_call_logs.map(&:id), status: :ok
    end

    private
    def verify_request
      begin
        @call_log = current_account.call_logs(true).find(params[:id])
      rescue
        render json: "The call log is not found".to_json, status: :not_found
        return
      end
    end
  end
end
