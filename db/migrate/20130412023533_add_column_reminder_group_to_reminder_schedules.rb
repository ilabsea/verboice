class AddColumnReminderGroupToReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :reminder_group_id, :integer, {default: nil}
  end
end
