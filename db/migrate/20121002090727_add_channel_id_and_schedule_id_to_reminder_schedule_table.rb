class AddChannelIdAndScheduleIdToReminderScheduleTable < ActiveRecord::Migration
  def change
  	add_column :ext_reminder_schedules, :channel_id  , :integer
  	add_column :ext_reminder_schedules, :schedule_id , :integer
  	add_column :ext_reminder_schedules, :timezone    , :string
  end
end
