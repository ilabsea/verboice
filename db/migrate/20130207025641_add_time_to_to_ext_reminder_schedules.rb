class AddTimeToToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :time_to, :datetime
  end
end
