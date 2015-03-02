class AddStatusToChannels < ActiveRecord::Migration
  def change
    add_column :channels, :status, :string, default: Channel::STATUS_APPROVED
  end
end
