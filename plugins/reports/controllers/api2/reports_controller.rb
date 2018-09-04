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
module Api2
  class ReportsController < Api2Controller

    # GET /call_logs
    def index
      @call_logs = api_current_account.call_logs
      @call_log_ids = @call_logs.map {|c| c.id }
      @reports = Report.joins(:call_log).where("reports.call_id in (#{@call_log_ids.join(',')})").select(["reports.call_id","call_logs.project_id","message", "properties", "location", "address", "call_logs.created_at"])
      @reports = @reports.where("reports.location = '#{params[:location]}'") unless params[:location].to_s.empty?
      @reports = @reports.where("call_logs.address = '#{params[:phone_number]}'") unless params[:phone_number].to_s.empty?
      unless (params[:from].to_s.empty? and params[:to].to_s.empty?)
        from = Time.parse(params[:from]).to_s
        to = Time.parse(params[:to]).to_s
        @reports = @reports.where("call_logs.created_at between '#{from}' AND '#{to}'") 
      end
      render json: @reports
    end
  end
end
