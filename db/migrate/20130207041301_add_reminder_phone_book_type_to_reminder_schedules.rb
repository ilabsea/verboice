class AddReminderPhoneBookTypeToReminderSchedules < ActiveRecord::Migration
  def change
    add_column :ext_reminder_schedules, :reminder_phone_book_type_id, :integer
  end
end
