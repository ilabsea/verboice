every 1.day, :at => '0:00 am' do
  rake "reminder:schedule"
  rake "download_zip:clear"
end

every "0 1 1 * *" do
  rake "backup:full"
end

every :day, :at => '3:00 am' do
  rake "backup:incremental"
end
