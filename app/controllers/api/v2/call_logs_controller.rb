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
  module V2
    class CallLogsController < ApiController
      before_filter :verify_request

      # GET /call_logs/:id
      def show
        render json: @call_log
      end

      private
      def verify_request
        begin
          @call_log = current_account.call_logs(true).find(params[:id])
        rescue
          render json: "The call log is not found".to_json, status: :not_found
          return
        end
      end

    end
  end
end