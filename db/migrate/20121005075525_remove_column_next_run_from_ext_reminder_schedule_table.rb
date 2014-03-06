class RemoveColumnNextRunFromExtReminderScheduleTable < ActiveRecord::Migration
  def up
  	remove_column  :ext_reminder_schedules, :next_run
  end
  
  def down
  	add_column :ext_reminder_schedules, :next_run, :boolean, :default => false
  end
end
