class AddUniqueIndexToCallLogAnswer < ActiveRecord::Migration
  def change
    add_index :call_log_answers, [:project_variable_id, :call_log_id], unique: true
  end
end
