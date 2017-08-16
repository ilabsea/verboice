namespace :users do
  desc "Export user as csv"
  task :export, [:path] => :environment do |t, args|
    puts "exporting user to #{args[:path]}"
    if args[:path].present?
      CSV.open(args[:path], "wb") do |csv|
        csv << ["email", "password", "pepper"]
        Account.find_each do |account|
          csv << [account.email, account.encrypted_password, Account.pepper]
        end
      end
    end
  end
end
