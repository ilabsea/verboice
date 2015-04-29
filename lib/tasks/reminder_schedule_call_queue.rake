
namespace :reminder do
	desc "schedule calls to all reminder schedule in projects"
	task :schedule => :environment do
		now = DateTime.now.utc

    Log.info(:reminder_schedule_log_dir, "====== started at #{now.to_s} ======")
    Project.all.each do |project|
      Log.info(:reminder_schedule_log_dir, "started project:#{project.id}")
      Ext::ReminderSchedule.schedule project.id, now
      Log.info(:reminder_schedule_log_dir, "finished project:#{project.id}")
    end
    Log.info(:reminder_schedule_log_dir, "====== finished at #{DateTime.now.utc.to_s} ======")

	end

  desc "migrate reminder schedule to support channel-suggestion"
  task :migrate_channel => :environment do
    Ext::ReminderSchedule.channel_migrate_reminder_schedule
  end

end