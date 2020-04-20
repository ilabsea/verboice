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
  class ReminderGroupsController < ApiController
    before_filter :validate_record, only: [:update, :destroy, :register, :deregister]
    before_filter :validate_project, only: [:index, :create]

    # GET /api/projects/:project_id/reminder_groups
    def index
      render json: @reminder_groups, root: false
    end

    # POST /api/projects/:project_id/reminder_groups
    def create
      params[:reminder_group][:addresses] = params[:reminder_group][:addresses].map(&:to_s).uniq if params[:reminder_group] && params[:reminder_group][:addresses].kind_of?(Array)
      @reminder_group = @reminder_groups.build params[:reminder_group]
      
      if params[:reminder_group].present? && params[:reminder_group][:addresses].present? && !params[:reminder_group][:addresses].kind_of?(Array)
        bad_request_invalid_array_parameter 'creating'
        return
      end

      if @reminder_group.save
        render json: @reminder_group, status: :created
      else
        render json: errors_to_json(@reminder_group, 'creating'), status: :bad_request
      end
    end

    # PUT /api/projects/:project_id/reminder_groups/:id
    def update
      if params[:reminder_group].present? && params[:reminder_group][:addresses].present? && !params[:reminder_group][:addresses].kind_of?(Array)
        bad_request_invalid_array_parameter 'updating'
        return
      end

      params[:reminder_group][:addresses] = params[:reminder_group][:addresses].map(&:to_s).uniq if params[:reminder_group] && params[:reminder_group][:addresses].kind_of?(Array)
      if @reminder_group.update_attributes(params[:reminder_group])
        render json: @reminder_group
      else
        render json: errors_to_json(@reminder_group, 'updating'), status: :bad_request
      end
    end

    # DELETE /api/projects/:project_id/reminder_groups/:id
    def destroy
      if @reminder_group.destroy
        render json: @reminder_group
      else
        render json: errors_to_json(@reminder_group, 'deleting'), status: :bad_request
      end
    end

    # PUT /api/projects/:project_id/reminder_groups/:id/register
    def register
      if(params[:address].blank?) or !params[:address].number?
        render json: 'Parameter address is missing', status: :bad_request
      else
        @reminder_group.register_address params[:address]
        render json: @reminder_group
      end
    end

    # PUT /api/projects/:project_id/reminder_groups/:id/deregister
    def deregister
      if(params[:address].blank?) or !params[:address].number?
        render json: 'Parameter address is missing', status: :bad_request
      else
        if @reminder_group.deregister_address params[:address]
          render json: @reminder_group
        else
          render json: "There is no address: #{params[:address]}", status: :no_content
        end
      end
    end

    private

    def bad_request_invalid_array_parameter action
      response = errors_to_json(@reminder_group, action)
      response[:properties].push({addresses: "Attribute was supposed to be a Array, but was a String"})
      render json: response, status: :bad_request
    end

    def validate_project
      begin
        load_project
        @reminder_groups = @project.ext_reminder_groups if @project
      rescue
        render json: "The project is not found".to_json, status: :not_found
        return
      end
    end

    def validate_record
      begin
        load_project
        @reminder_group = @project.ext_reminder_groups.find(params[:id]) if @project
      rescue
        render json: "The reminder group is not found".to_json, status: :not_found
        return
      end
    end

  end
end