class ChangeReminderGroupAddressesToBlob < ActiveRecord::Migration
  def up
    change_column :ext_reminder_groups, :addresses, :blob
  end

  def down
    change_column :ext_reminder_groups, :addresses, :string
  end
end
