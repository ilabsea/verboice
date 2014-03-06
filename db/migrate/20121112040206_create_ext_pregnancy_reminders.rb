class CreateExtPregnancyReminders < ActiveRecord::Migration
  def change
    create_table :ext_pregnancy_reminders do |t|
      t.string :name
      t.string :description
      t.integer :day
      t.string :queued_call

      t.references :call_flow
      t.references :project
      t.references :channel
      t.references :schedule

      t.timestamps
    end
    add_index :ext_pregnancy_reminders, :call_flow_id
    add_index :ext_pregnancy_reminders, :project_id
    add_index :ext_pregnancy_reminders, :channel_id
    add_index :ext_pregnancy_reminders, :schedule_id
  end
end
