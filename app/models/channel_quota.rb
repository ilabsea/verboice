class ChannelQuota < ActiveRecord::Base
  belongs_to :channel
  attr_accessible :blocked, :enabled, :total, :used

  validates_uniqueness_of :channel_id

end
