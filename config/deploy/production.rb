set :user, 'ilab'
set :branch, 'stable-merge-bitbucket'

server '192.168.1.100', :app, :web, :db, primary: true
