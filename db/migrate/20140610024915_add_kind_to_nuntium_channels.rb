class AddKindToNuntiumChannels < ActiveRecord::Migration
  def change
    add_column :nuntium_channels, :kind, :string, default: nil
  end
end
