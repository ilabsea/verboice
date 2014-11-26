set :user, 'ilab'
set :server, "192.168.1.53"
set :branch, 'development'

server "192.168.1.53", :app, :web, :db, primary: true
