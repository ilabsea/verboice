class AddStoreCallLogEntriesToProject < ActiveRecord::Migration
  def change
    add_column :projects, :store_call_log_entries, :boolean, default: true
  end
end
