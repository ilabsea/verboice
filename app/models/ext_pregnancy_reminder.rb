class ExtPregnancyReminder < ActiveRecord::Base
  belongs_to :call_flow
  belongs_to :project
  belongs_to :channel
  belongs_to :schedule
  # attr_accessible :title, :body
end
