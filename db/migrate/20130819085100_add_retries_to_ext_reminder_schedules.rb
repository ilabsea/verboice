class AddRetriesToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :retries, :boolean, :default => false
  end
end
