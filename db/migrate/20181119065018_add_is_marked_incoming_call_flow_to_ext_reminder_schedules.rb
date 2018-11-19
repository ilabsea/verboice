class AddIsMarkedIncomingCallFlowToExtReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :is_marked_incoming_call_flow, :boolean, default: false
  end
end
