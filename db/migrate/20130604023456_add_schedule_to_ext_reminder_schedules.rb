class AddScheduleToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :schedule, :text, :default => nil
  end
end
