class MigrateReminderScheduleToNewUi < ActiveRecord::Migration
  def up
    Ext::ReminderSchedule.all.each do |reminder_schedule|
      # rewrite repeat wday with condition
      if reminder_schedule.repeat? && !reminder_schedule.conditions.empty?
        reminder_schedule.reset_repeat_on_specific_day_with_condition_to_condition!
      end
      
      # repeat wday without condition
      if reminder_schedule.repeat? && reminder_schedule.conditions.empty?
        reminder_schedule.reset_repeat_everyday_to_one_time!
      end
      
      # one time on specific date with conditions
      if !reminder_schedule.repeat? && !reminder_schedule.conditions.empty?
        reminder_schedule.reset_one_time_with_condition_to_condition!
      end
    end
  end

  def down
  end
end
