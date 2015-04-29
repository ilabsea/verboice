namespace :backup do
  desc "Full backup"
  task :full => :environment do
    Backup.full!
  end

  desc "Incremential backup"
  task :incremental => :environment do
    Backup.incremental!
  end

  desc "Restore backup"
  task :restore => :environment do
    Backup.restore!
  end
end