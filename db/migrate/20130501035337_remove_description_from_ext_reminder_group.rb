class RemoveDescriptionFromExtReminderGroup < ActiveRecord::Migration
  def up
    remove_column :ext_reminder_groups, :description
  end

  def down
    add_column :ext_reminder_groups, :description, :string
  end
end
