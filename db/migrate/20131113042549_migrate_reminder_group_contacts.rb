class MigrateReminderGroupContacts < ActiveRecord::Migration
  def up
    database_config_file = File.join(Rails.root, "config", "database.yml")
    database_yml = YAML::load(File.open(database_config_file))[Rails.env]
    begin
      db_name = "#{database_yml['database']}_bak"
      db_bak = Mysql2::Client.new(host: "localhost", username: database_yml["username"], password: database_yml["password"], database: db_name)
      resultset = db_bak.query("select * from ext_reminder_groups")
      resultset.each do |r|
        say "id: #{r['id']}"
        if not r['addresses'].nil?
          addresses = YAML.load(r['addresses'])
          say "addresses: #{r['addresses']}"
          if addresses.kind_of?(Array)
            reminder_group = Ext::ReminderGroup.find(r['id']) rescue next
            addresses.each do |address|
              say "migrate address: #{address} to reminder group"
              reminder_group.register_address address
            end
          end
        end
      end
    rescue
      say "There is no database #{db_name}"
    end
  end

  def down
  end
end
