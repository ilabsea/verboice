class RemoveColumnReminderPhoneBookTypeFromReminderSchedules < ActiveRecord::Migration
  def up
    # migrate reminder_phone_book_type to reminder_group
    Ext::ReminderPhoneBookType.all.each do |type|
      addresses = type.reminder_phone_books.map &:phone_number
      reminder_group = Ext::ReminderGroup.create(name: type.name, project_id: type.project_id, addresses: addresses)
      Ext::ReminderSchedule.where(reminder_phone_book_type_id: type.id).each do |schedule|
        schedule.reminder_group = reminder_group
        schedule.save
      end
    end

    remove_column :ext_reminder_schedules, :reminder_phone_book_type_id
  end

  def down
    add_column :ext_reminder_schedules, :reminder_phone_book_type_id, :integer

    # migrate reminder_phone_book_type to reminder_group
    Ext::ReminderPhoneBookType.all.each do |type|
      reminder_groups = Ext::ReminderGroup.where(name: type.name, project_id: type.project_id)
      reminder_groups.each do |reminder_group|
        Ext::ReminderSchedule.where(reminder_group_id: reminder_group.id).each do |schedule|
          schedule.reminder_phone_book_type = type
          schedule.save
        end
        reminder_group.destroy
      end
    end
  end
end