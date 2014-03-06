class AddRetriesInHoursToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :retries_in_hours, :string, :default => nil
  end
end
