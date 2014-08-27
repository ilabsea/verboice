class CreateChannelQuotas < ActiveRecord::Migration
  def change
    create_table :channel_quotas do |t|
      t.references :channel
      t.boolean :enabled, default: false
      t.boolean :blocked, default: false
      
      t.timestamps
    end
    add_index :channel_quotas, :channel_id
  end
end
