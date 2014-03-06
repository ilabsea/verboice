class RemoveDescriptionFromExtReminderSchedules < ActiveRecord::Migration
  def up
    remove_column :ext_reminder_schedules, :description
  end

  def down
    add_column :ext_reminder_schedules, :description, :string
  end
end
