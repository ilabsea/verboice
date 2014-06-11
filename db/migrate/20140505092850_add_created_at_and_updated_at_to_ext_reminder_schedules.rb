class AddCreatedAtAndUpdatedAtToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :created_at, :datetime
    add_column :ext_reminder_schedules, :updated_at, :datetime
  end
end
