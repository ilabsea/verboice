class AddSyncGoDataToTableReminderGroups < ActiveRecord::Migration
  def change
    add_column :ext_reminder_groups, :mode, :string
    add_column :ext_reminder_groups, :enabled_synced, :boolean
    add_column :ext_reminder_groups, :endpoint, :string
    add_column :ext_reminder_groups, :username, :string
    add_column :ext_reminder_groups, :password, :string
    add_column :ext_reminder_groups, :synced_schedule, :string
  end
end
