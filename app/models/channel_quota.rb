class ChannelQuota < ActiveRecord::Base
  set_table_name 'channel_quotas'
  
  belongs_to :channel
  attr_accessible :blocked, :enabled

  validates_uniqueness_of :channel_id

end
