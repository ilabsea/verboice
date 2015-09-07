set :user, 'ilab'

# set :server, 'verboice-cambodia.instedd.org'
# set :port, '23456'
# set :branch, 'master'

# server 'verboice-cambodia.instedd.org', :app, :web, :db, primary: true


set :server, '192.168.1.134' # '110.74.193.186'
# set :port, '8083'
set :branch, 'release-2.0'
server '192.168.1.134', :app, :web, :db, primary: true 