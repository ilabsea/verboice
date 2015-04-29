class AddTypeIdToExtReminderPhoneBooks < ActiveRecord::Migration
  def change
    add_column :ext_reminder_phone_books, :type_id, :integer
  end
end
