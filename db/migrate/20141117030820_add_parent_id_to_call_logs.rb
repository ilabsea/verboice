class AddParentIdToCallLogs < ActiveRecord::Migration
  def change
    add_column :call_logs, :parent_id, :integer, default: nil
  end
end
