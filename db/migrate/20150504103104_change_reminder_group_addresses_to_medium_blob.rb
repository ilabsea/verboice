class ChangeReminderGroupAddressesToMediumBlob < ActiveRecord::Migration
  def up
    change_column :ext_reminder_groups, :addresses, :mediumblob
  end

  def down
    change_column :ext_reminder_groups, :addresses, :blob
  end
end
