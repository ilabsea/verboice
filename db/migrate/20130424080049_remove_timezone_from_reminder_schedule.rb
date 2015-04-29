class RemoveTimezoneFromReminderSchedule < ActiveRecord::Migration
  def up
  	remove_column :ext_reminder_schedules, :timezone
  end

  def down
  	add_column :ext_reminder_schedules, :timezone, :string, :default => nil
  end
end
