class AddSyncGoDataToTableReminderGroups < ActiveRecord::Migration
  def change
    add_column :ext_reminder_groups, :mode, :string
    add_column :ext_reminder_groups, :enable_sync, :boolean
    add_column :ext_reminder_groups, :sync_config, :text
    add_column :ext_reminder_groups, :sync_status_updated_at, :datetime
  end
end
