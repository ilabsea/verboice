class AddRetriesToCallLogs < ActiveRecord::Migration
  def change
    add_column :call_logs, :retries, :integer, null: false, default: 0
  end
end
