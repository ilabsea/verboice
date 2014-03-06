class AddStartedAtToExtPregnancyReminders < ActiveRecord::Migration
  def change
    add_column :ext_pregnancy_reminders, :started_at, :datetime, :default => nil
  end
end
