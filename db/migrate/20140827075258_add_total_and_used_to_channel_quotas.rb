class AddTotalAndUsedToChannelQuotas < ActiveRecord::Migration
  def change
    add_column :channel_quotas, :total, :float, default: 0
    add_column :channel_quotas, :used, :float, default: 0
  end
end
