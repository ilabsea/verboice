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
  class ReminderSchedulesController < Api2Controller

    # GET /api/reminder_schedules
    def index
      load_project
      @reminder_schedules = @project.ext_reminder_schedules
      render json: @reminder_schedules, each_serializer: CustomReminderScheduleSerializer
    end

    def create
      load_project
      @reminder_schedule = @project.ext_reminder_schedules.build(params[:reminder_schedule])
      if(@reminder_schedule.save)
        render json: @reminder_schedule, serializer: CustomReminderScheduleSerializer, status: 201
      else
        response_with_bad_request
      end
    end

    def update
      load_project
      @reminder_schedules = @project.ext_reminder_schedules.find(params[:id])
      if(@reminder_schedules.update_reminder_schedule_with_queues_call(params[:reminder_schedule]))
        render json: @reminder_schedules, serializer: CustomReminderScheduleSerializer
      else
        response_with_bad_request
      end
    end

    def destroy
      load_project
      @reminder_schedules = @project.ext_reminder_schedules.find(params[:id])
      if @reminder_schedules.destroy
        head :ok
      else
        response_with_bad_request
      end
    end

    private

    def load_project
      @project = @project || Project.find(params[:project_id])
    end

  end
end