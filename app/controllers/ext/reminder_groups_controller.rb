module Ext
  class ReminderGroupsController < ExtApplicationController
    before_filter :load_project, only: [:index]

    def index
      respond_to do |format|
        format.html
        format.json { render json: @project.ext_reminder_groups, each_serializer: ReminderGroupSerializer }
      end
    end

    def import
      if params[:file_name].blank?
        redirect_to({:action => :index}, :flash => {:alert => I18n.t("controllers.ext.reminder_groups_controller.no_file_found")})
      else
        begin
          extension = File.extname params[:file_name].original_filename
          load_project
          @reminder = @project.ext_reminder_groups.find(params[:id])
          case extension
          when '.csv'
            @reminder.import_contact_addresses(params[:file_name])
          else
            raise I18n.t("controllers.ext.reminder_groups_controller.invalid_extension")
          end
          redirect_to({ :action => :index }, {:notice => I18n.t("controllers.ext.reminder_groups_controller.successfully_updated", :reminder_group_name => @reminder.name)})
        rescue Exception => ex
          redirect_to({:action => :index}, :flash => {:error => I18n.t("controllers.ext.reminder_groups_controller.invalid_file", :ex => ex)})
        end
      end
    end

  end
end
