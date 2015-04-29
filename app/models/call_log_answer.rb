class CallLogAnswer < ActiveRecord::Base
  belongs_to :call_log
  belongs_to :project_variable
  attr_accessible :project_variable_id, :value, :call_log_id
  validates_uniqueness_of :project_variable_id, :scope => :call_log_id
end
