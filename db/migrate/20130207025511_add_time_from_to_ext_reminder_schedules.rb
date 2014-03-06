class AddTimeFromToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :time_from, :datetime
  end
end
