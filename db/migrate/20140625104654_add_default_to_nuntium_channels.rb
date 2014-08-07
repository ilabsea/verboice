class AddDefaultToNuntiumChannels < ActiveRecord::Migration
  def change
    add_column :nuntium_channels, :default, :boolean, default: false
  end
end
