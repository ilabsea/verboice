class RenameDayToWeekInExtPregnancyReminders < ActiveRecord::Migration
  def up
    rename_column :ext_pregnancy_reminders, :day, :week
  end

  def down
    rename_column :ext_pregnancy_reminders, :week, :day
  end
end
