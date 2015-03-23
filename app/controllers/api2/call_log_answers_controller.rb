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
  class CallLogAnswersController < Api2Controller

    # GET api2/call_log_answers
    def index
      project = api_current_account.projects.find(params[:project_id])
      variable = project.project_variables.find(params[:variable_id])
      call_log_answers = CallLogAnswer.where(project_variable_id: variable.id)
        .where("updated_at >= '#{params[:from]}' && updated_at <= '#{params[:to]}'").includes(:call_log)
      render json: call_log_answers, each_serializer: CustomCallLogAnswerSerializer
    end

  end
end
