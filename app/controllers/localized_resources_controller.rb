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

class LocalizedResourcesController < ApplicationController

  expose(:project) { current_account.projects.find(params[:project_id]) }
  expose(:resource) { project.resources.find(params[:resource_id]) }
  expose(:localized_resources) { resource.localized_resources }
  expose(:localized_resource)

  skip_before_filter :verify_authenticity_token, :only => [:save_recording, :save_file]

  include AudioUtils

  def save_recording
    RecordingManager.remove_audio(localized_resource.guid)
    localized_resource.recorded_audio = request.body.read
    localized_resource.save
    head :ok
  end

  def play_recording
    send_data localized_resource.recorded_audio
  end

  def save_file
    RecordingManager.remove_audio(localized_resource.guid)
    localized_resource.filename = "#{params[:filename]}" if params[:filename].present?
    localized_resource.uploaded_audio = convert_to_8000_hz_wav(request.body.read, request.content_type)
    localized_resource.save
    if params[:filename].present? && request.content_type.audio_mime_type?
      render :text => "OK"
    else
      render :text => I18n.t("controllers.localized_resources_controller.invalid_audio_file")
    end
  end

  def play_file
    send_data localized_resource.uploaded_audio, :filename => localized_resource.filename
  end

end
