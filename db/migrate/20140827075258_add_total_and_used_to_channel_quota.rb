class AddTotalAndUsedToChannelQuota < ActiveRecord::Migration
  def change
    add_column :channel_quota, :total, :float, default: 0
    add_column :channel_quota, :used, :float, default: 0
  end
end
