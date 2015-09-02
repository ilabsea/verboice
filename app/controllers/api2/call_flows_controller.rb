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
  class CallFlowsController < Api2Controller
    expose(:call_flows) { api_current_account.call_flows }

    def list
      if params[:project_id].present?
        call_flows = api_current_account.projects.find(params[:project_id]).call_flows
      end

      render json: call_flows, each_serializer: CustomCallFlowSerializer
    end

    def show
      call_flow = call_flows.find(params[:id])
      
      render json: call_flow, serializer: CustomCallFlowSerializer, flow: true
    end
  end
end