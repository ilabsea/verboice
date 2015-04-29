class AllowRetriesNullInCallLogs < ActiveRecord::Migration
  def up
    change_column :call_logs, :retries, :integer, null: true, default: 0
  end

  def down
    change_column :call_logs, :retries, :integer, null: false, default: 0
  end
end
