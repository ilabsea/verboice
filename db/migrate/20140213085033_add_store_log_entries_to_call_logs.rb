class AddStoreLogEntriesToCallLogs < ActiveRecord::Migration
  def up
    add_column :call_logs, :store_log_entries, :boolean
    execute 'UPDATE call_logs SET store_log_entries = 1'
  end

  def down
    remove_column :call_logs, :store_log_entries
  end
end
