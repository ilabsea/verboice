class Report < ActiveRecord::Base
  belongs_to :call_log, foreign_key: "call_id"
  serialize :properties, Array

  
end
