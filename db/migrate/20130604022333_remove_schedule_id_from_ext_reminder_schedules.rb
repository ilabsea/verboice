class RemoveScheduleIdFromExtReminderSchedules < ActiveRecord::Migration
  def up
    remove_column :ext_reminder_schedules, :schedule_id
  end

  def down
    add_column :ext_reminder_schedules, :schedule_id, :integer, :default => nil
  end
end
