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
  class CallLogsController < Api2Controller
    before_filter :verify_request, :only => [:show]
    before_filter :filter, :only => [:list_by_channel]

    # GET /call_logs
    def index
      if true or api_current_account.admin?
        if true or api_current_account.has_access_from?(origin_host)
          @call_logs = CallLog.where("1=1")
          @call_logs = @call_logs.by_account_id(params[:account_id]) if params[:account_id]
          @call_logs = @call_logs.by_channel_id(params[:channel_id]) if params[:channel_id]
          if params[:channels_id].present?
            ids = params[:channels_id].split(",")
            @call_logs = @call_logs.where(["channel_id in (?) ", ids])
          end
          @call_logs = @call_logs.between(params[:start_date], params[:end_date])
        else
          return head :unauthorized
        end
      else
        @call_logs = api_current_account.call_logs
        @call_logs = @call_logs.where(address: params[:address]) if params[:address]
      end

      @call_logs = @call_logs.order('channel_id').includes([:account, :channel, :project])

      render json: @call_logs, each_serializer: CustomCallLogSerializer
    end

    # GET /call_logs/:id
    def show
      render json: @call_log, serializer: CustomCallLogSerializer
    end

    # GET /channels/:channel_id/call_logs
    def list_by_channel
      render json: @call_logs, each_serializer: CustomCallLogSerializer
    end

    private

    def verify_request
      begin
        @call_log = api_current_account.call_logs.find(params[:id])
      rescue
        head :not_found
        return
      end
    end

    def filter
      status_code = nil
      if api_current_account.admin? && api_current_account.has_access_from?(origin_host)
        @channel = Channel.find(params[:channel_id]) rescue status_code = :not_found
      else
        @channel = api_current_account.channels.find(params[:channel_id]) rescue status_code = :unauthorized
      end

      return head status_code if status_code

      @call_logs = @channel.call_logs

      if params[:start_date].present? && params[:end_date].present?
        @call_logs = @call_logs.between(params[:start_date],params[:end_date])
      end

      @call_logs

    end

  end
end
