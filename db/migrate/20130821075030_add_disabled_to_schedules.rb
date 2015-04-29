class AddDisabledToSchedules < ActiveRecord::Migration
  def change
    add_column :schedules, :disabled, :boolean, :default => false
  end
end
