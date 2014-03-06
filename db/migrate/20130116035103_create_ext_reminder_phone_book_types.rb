class CreateExtReminderPhoneBookTypes < ActiveRecord::Migration
  def change
    create_table :ext_reminder_phone_book_types do |t|
      t.string :name
      t.string :description
      t.references :project

      t.timestamps
    end
    add_index :ext_reminder_phone_book_types, :project_id
  end
end
