class AddDurationToCallLogs < ActiveRecord::Migration
  def up
    add_column :call_logs, :duration, :integer, null: false, default: 0
  end
  
  def down
    remove_column :call_logs, :duration
  end
end
