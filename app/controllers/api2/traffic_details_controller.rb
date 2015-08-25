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
  class TrafficDetailsController < Api2Controller
    before_filter :validate_params, only: [:index]

    def index
      traffics = CallLog.between(@start_date, @end_date)

      if params[:project_id].present?
        traffics = traffics.where(project_id: params[:project_id])
      end

      if params[:channel_id].present?
        traffics = traffics.where(channel_id: params[:channel_id])
      end

      if params[:call_flow_id].present?
        traffics = traffics.where(call_flow_id: params[:call_flow_id])
      end

      traffics = traffics.select("channel_id, state, fail_reason, sum(duration) as total_duration, count(*) as total_call").group("channel_id, state, fail_reason").order(account_id: :asc)

      render json: traffics, each_serializer: CustomTrafficDetailSerializer
    end

    private

    def validate_params
      return head :unprocessable_entity unless params[:start_date]
      return head :unprocessable_entity unless params[:end_date]

      begin
        @start_date = DateTime.parse(params[:start_date])
        @end_date = DateTime.parse(params[:end_date])
      rescue
        return head :unprocessable_entity
      end
    end
  end
end
