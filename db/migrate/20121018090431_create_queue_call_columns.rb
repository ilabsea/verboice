class CreateQueueCallColumns < ActiveRecord::Migration
  def change
  	add_column :ext_reminder_schedules, :queue_call_id, :string
  end
end
