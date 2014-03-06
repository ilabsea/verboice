class AddAnnotationToCallLogRecordedAudios < ActiveRecord::Migration
  def change
    add_column :call_log_recorded_audios, :annotation, :string
  end
end
