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
  class ReportsController < Api2Controller

    # GET /call_logs
    def index
      reports = Report.order('id DESC').limit(Reports::Settings.default_max_fetch)
      reports = reports.where(location: params[:location]) if params[:location].present?
      reports = reports.where(address: params[:phone_number]) if params[:phone_number].present?

      if params[:from].present?
        begin
          from = Time.parse(params[:from])
          reports = reports.where("created_at >= ?", from)
        rescue
          Rails.logger.info "Invalid time - from: #{params[:from]}"
        end
      end

      if params[:to].present?
        begin
          to = Time.parse(params[:to])
          reports = reports.where("created_at <= ?", to)
        rescue
          Rails.logger.info "Invalid time - to: #{params[:to]}"
        end
      end

      render json: reports
    end
  end
end
