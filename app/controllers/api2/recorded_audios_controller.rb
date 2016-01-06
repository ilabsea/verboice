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
  class RecordedAudiosController < Api2Controller

    # GET /call_logs/:call_log_id/audios
    def index
      call_log = api_current_account.call_logs.find_by_id(params[:call_log_id])
      recorded_audios = call_log.nil? ? [] : call_log.recorded_audios

      render json: recorded_audios, each_serializer: CustomRecordedAudioSerializer
    end

    # GET /call_logs/:call_log_id/audios/:filename
    def play
      log = current_account.call_logs.includes(:recorded_audios).find(params[:call_log_id])
      recorded_audio = log.recorded_audios.find_by_key(params[:filename])
      if recorded_audio
        send_file recorded_audio.url, :type => "audio/x-wav"
      else
        render status: :not_found, text: ""
      end
    end

  end
end
