class RenameQueuedCallToQueuedCallIdsInExtPregnancyReminders < ActiveRecord::Migration
  def up
    rename_column :ext_pregnancy_reminders, :queued_call, :queued_call_ids
  end

  def down
    rename_column :ext_pregnancy_reminders, :queued_call_ids, :queued_call
  end
end
