class CreateCallLogAnswers < ActiveRecord::Migration
  def change
    create_table :call_log_answers do |t|
      t.integer :project_variable_id
      t.string :value
      t.references :call_log

      t.timestamps
    end
    add_index :call_log_answers, :call_log_id
  end
end
