require 'spec_helper'

describe ChannelQuota do
  it { should validate_uniqueness_of(:channel_id) }
end
