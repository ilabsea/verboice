require 'spec_helper'

describe Ext::ReminderChannel do

  it { should validate_uniqueness_of(:channel_id).scoped_to(:reminder_schedule_id) }

end
