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
  class UploadLocalizedResourcesController < ApiController

    include AudioUtils

    # GET /api/call_flows/:call_flow_id/audio_resources
    def index
      call_flow = current_account.find_call_flow_by_id(params[:call_flow_id])
      resources = {
        project_id: call_flow.project.id,
        call_flow_id: call_flow.id,
        call_flow_name: call_flow.name,
        resources: call_flow.audio_resources
      }

      render json: resources
    end

    # PUT /api/call_flows/:call_flow_id/audio_resources/:id
    def update
      call_flow = current_account.find_call_flow_by_id(params[:call_flow_id])

      localized_resource = call_flow.project.localized_resources.find(params[:id])

      localized_resource.filename = "#{params[:filename]}" if params[:filename].present?
      localized_resource.uploaded_audio = convert_to_8000_hz_wav(request.body.read, request.content_type)

      localized_resource.save

      if params[:filename].present? && request.content_type.audio_mime_type?
        render json: {success: true, error_message: ""}
      else
        render json: {success: false, error_message: I18n.t("controllers.localized_resources_controller.invalid_audio_file")}
      end
    end

  end
end