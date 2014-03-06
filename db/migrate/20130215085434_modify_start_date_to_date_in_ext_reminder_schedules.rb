class ModifyStartDateToDateInExtReminderSchedules < ActiveRecord::Migration
  def up
    change_column :ext_reminder_schedules, :start_date, :date
  end

  def down
    change_column :ext_reminder_schedules, :start_date, :datetime
  end
end
