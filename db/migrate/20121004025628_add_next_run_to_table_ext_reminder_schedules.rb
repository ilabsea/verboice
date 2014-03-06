class AddNextRunToTableExtReminderSchedules < ActiveRecord::Migration
  def change
  	add_column :ext_reminder_schedules, :next_run, :boolean, :default => false
  end
end
