class CreateChannelQuota < ActiveRecord::Migration
  def change
    create_table :channel_quota do |t|
      t.references :channel
      t.boolean :enabled, default: false
      t.boolean :blocked, default: false
      
      t.timestamps
    end
    add_index :channel_quota, :channel_id
  end
end
