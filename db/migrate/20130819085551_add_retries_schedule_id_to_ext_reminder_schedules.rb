class AddRetriesScheduleIdToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :retries_schedule_id, :integer, :default => nil
  end
end
