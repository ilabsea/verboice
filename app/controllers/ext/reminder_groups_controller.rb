module Ext
  class ReminderGroupsController < ExtApplicationController
    def index
      load_project
      groups = []
      @project.ext_reminder_groups.each do |reminder_group|
        if reminder_group.addresses.kind_of?(String)
          reminder_group.addresses = Ext::ReminderGroup.deserialized_to_array reminder_group.addresses
          reminder_group.save
        end
        groups.push reminder_group
      end

      respond_to do |format|
        format.html
        format.json { render json: groups }
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
            CSV.parse params[:file_name].read do |row|
              next if !row[0].strip.is_contact?
              @reminder.register_address row[0].to_number
            end
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
