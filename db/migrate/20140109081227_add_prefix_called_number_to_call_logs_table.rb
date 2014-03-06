class AddPrefixCalledNumberToCallLogsTable < ActiveRecord::Migration
  def change
  	add_column :call_logs, :prefix_called_number, :string
  end
end
