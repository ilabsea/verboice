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
  class CallFlowTracesController < Api2Controller
    before_filter :validate_params, only: [:index]

    def index
      traces = Trace.joins(:call_log).where(call_flow_id: @call_flow.id)

      traces = traces.where(call_logs: { channel_id: params[:channel_id] }) if params[:channel_id].present?

      traces = traces.where("traces.created_at >= '#{@start_date}'") if params[:start_date].present?

      traces = traces.where("traces.created_at <= '#{@end_date}'") if params[:end_date].present?

      traces = traces.select("step_id, step_name, count(*) as total").group("step_id").order("total desc")

      render json: traces, each_serializer: CustomCallFlowTraceSerializer
    end

    private

    def validate_params
      return head :unprocessable_entity unless params[:call_flow_id]

      begin
        @call_flow = api_current_account.call_flows.find(params[:call_flow_id])

        @start_date = DateTime.parse(params[:start_date]) if params[:start_date]
        @end_date = DateTime.parse(params[:end_date]) if params[:end_date]
      rescue
        return head :unprocessable_entity
      end

    end

  end
end