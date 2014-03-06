class AddTimeZoneToExtPregnancyReminders < ActiveRecord::Migration
  def change
    add_column :ext_pregnancy_reminders, :timezone, :string, :default => nil
  end
end
