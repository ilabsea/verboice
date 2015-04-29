class RemoveColumnNameFromExtReminderPhoneBooks < ActiveRecord::Migration
  def up
    remove_column :ext_reminder_phone_books, :name
  end

  def down
    add_column :ext_reminder_phone_books, :name, :string
  end
end
