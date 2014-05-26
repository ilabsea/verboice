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
  class TrafficController < Api2Controller
    before_filter :verify_authorized, :validate_params, only: [:index]

    def index
      start_date = Ext::Parser::DateParser.parse(params[:start_date])
      end_date = Ext::Parser::DateParser.parse(params[:end_date])
      call_logs = CallLog.where(started_at: start_date..end_date)
      call_logs = call_logs.select("started_at, account_id, channel_id, prefix_called_number, address, direction, sum(duration) as total_duration").group(:channel_id, :address, :direction).having("sum(duration) > 0")

      render json: call_logs, each_serializer: CustomTrafficSerializer
    end

    private

    def verify_authorized
      return head :unauthorized unless api_current_account.admin?
      return head :unauthorized if api_current_account.admin? && !api_current_account.has_access_from?(origin_host)
    end

    def validate_params
      return head :unprocessable_entity unless params[:start_date]
      return head :unprocessable_entity unless params[:start_date].date_format?
      return head :unprocessable_entity unless params[:end_date]
      return head :unprocessable_entity unless params[:end_date].date_format?
    end
  end
end