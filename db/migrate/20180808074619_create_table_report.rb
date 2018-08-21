class CreateTableReport < ActiveRecord::Migration
  def change
    create_table :reports do |t|
    	t.references :call
      t.text :properties
      t.string :location

      t.timestamps
    end
  end
end
