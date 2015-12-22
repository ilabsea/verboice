set :user, 'deploy'
set :branch, 'release-2.0'

server '192.168.1.134', :app, :web, :db, primary: true
