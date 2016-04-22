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
  class CallFlowsController < ApiController

    # GET /api/call_flows
    # GET /api/projects/:project_id/call_flows
    def index
      if params[:project_id].present?
        project = current_account.projects.includes(:call_flows).find(params[:project_id])
        call_flows = project.call_flows
      else
        call_flows = current_account.call_flows
      end

      render json: call_flows, each_serializer: CustomCallFlowSerializer
    end
  end
end