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

    # GET /contacts/:address/call_logs
    # GET /call_logs
    def index
      @call_logs = api_current_account.call_logs
      @call_logs = @call_logs.where(address: params[:address]) if params[:address]
      render json: @call_logs, root: false
    end

    # GET /call_logs/:id
    def show
      render json: @call_log
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

      @search = ""
      %w(start_date end_date).each do |key|
        @search << search_by_key(key)
      end

      @call_logs = @call_logs.search @search if @search.present?
    end

    def search_by_key(key)
      params[key].present? ? " #{search_key_of(key)}:\"#{params[key]}\"" : ""
    end

    def search_key_of(key)
      search_keys = { 'start_date' => 'after', 'end_date' => 'before' }
      search_keys[key]
    end

  end
end
