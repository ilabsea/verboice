class ModifyFromTimeAndToTimeToStringInExtReminderSchedules < ActiveRecord::Migration
  def up
    change_column :ext_reminder_schedules, :time_from, :string
    change_column :ext_reminder_schedules, :time_to, :string
  end

  def down
    change_column :ext_reminder_schedules, :time_from, :datetime
    change_column :ext_reminder_schedules, :time_to, :datetime
  end
end
