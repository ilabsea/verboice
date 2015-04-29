class CreateCallLogRecordedAudios < ActiveRecord::Migration
  def change
    create_table :call_log_recorded_audios do |t|
      t.references :call_log
      t.references :project_variable
      t.string :key
      t.string :description

      t.timestamps
    end
    add_index :call_log_recorded_audios, :call_log_id
    add_index :call_log_recorded_audios, :project_variable_id
  end
end
