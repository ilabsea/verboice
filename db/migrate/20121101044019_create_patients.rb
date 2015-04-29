class CreatePatients < ActiveRecord::Migration
  def change
    create_table :patients do |t|
      t.date :pregnancy_date
      t.references :reminder_phone_book

      t.timestamps
    end
    add_index :patients, :reminder_phone_book_id
  end
end
