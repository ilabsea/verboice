class AddConditionsToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :conditions, :string
  end
end
