class AddMessageTextToReport < ActiveRecord::Migration
  def change
  	add_column :reports, :message, :text
  end
end
