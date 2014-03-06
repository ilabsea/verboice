class CreateExtReminderChannels < ActiveRecord::Migration
  def change
    create_table :ext_reminder_channels do |t|
	  t.references :reminder_schedule
	  t.references :channel	
      t.timestamps
    end
  end
end
