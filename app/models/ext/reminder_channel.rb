module Ext
  class ReminderChannel < ExtActiveRecord
  	attr_accessible :reminder_schedule_id, :channel_id
    belongs_to :reminder_schedule, class_name: 'Ext::ReminderSchedule'
    belongs_to :channel, class_name: 'Channel'

    assign_has_many_to "Channel", :ext_reminder_channels, :class_name => "Ext::ReminderChannel", :dependent => :destroy
  end
end

