class ChangeQueueCallIdReminderScheduleToText < ActiveRecord::Migration
  def up
    change_column :ext_reminder_schedules, :queue_call_id, :text
  end

  def down
    change_column :ext_reminder_schedules, :queue_call_id, :string
  end
end
