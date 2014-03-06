class CallLogRecordedAudio < ActiveRecord::Base
  belongs_to :call_log
  belongs_to :project_variable

  attr_accessible :description, :annotation, :key, :call_log_id, :project_variable_id

  validates_presence_of :call_log_id
  validates_presence_of :project_variable_id

  validates_uniqueness_of :project_variable_id, :scope => :call_log_id
end
