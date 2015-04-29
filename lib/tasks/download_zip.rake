namespace :download_zip do
  desc "Clear old download zip"
  task :clear => :environment do
    Account.find_each do |account|
      account.clear_downloads
    end
  end
end