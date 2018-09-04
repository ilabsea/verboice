# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'bundler/capistrano'
require 'yaml'

if ENV['RVM']
  require 'rvm/capistrano'
  set :rvm_ruby_string, '1.9.3-p550'
  set :rvm_type, :system
else
  default_run_options[:shell] = "/bin/bash --login"
end

set :whenever_command, "bundle exec whenever"
require "whenever/capistrano"

set :stages, %w(production staging)
set :default_stage, :staging
require 'capistrano/ext/multistage'

set :application, "verboice"
set :repository,  "https://github.com/ilabsea/verboice"
set :scm, :git

set :deploy_to, "/u/apps/verboice"

set :deploy_via, :remote_cache

default_environment['TERM'] = ENV['TERM']

# role :web, "your web-server here"                          # Your HTTP server, Apache/etc
# role :app, "your app-server here"                          # This may be the same as your `Web` server
# role :db,  "your primary db-server here", :primary => true # This is where Rails migrations will run
# role :db,  "your slave db-server here"

namespace :deploy do
  task :start do ; end
  task :stop do ; end
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "#{try_sudo} touch #{File.join(current_path,'tmp','restart.txt')}"
  end

  task :prepare_broker, :roles => :app do
    run "test -f #{shared_path}/verboice.config || cp #{release_path}/broker/verboice.config #{shared_path}"
    run "ln -nfs #{shared_path}/verboice.config #{release_path}/broker/verboice.config"

    run "test -d #{shared_path}/log/broker || mkdir #{shared_path}/log/broker"
    run "ln -nfs #{shared_path}/log/broker #{release_path}/broker/log"
  end

  task :compile_broker, :roles => :app do
    run "make -C #{release_path}/broker"
  end

  task :symlink_configs, :roles => :app do
    %W(asterisk credentials freeswitch verboice voxeo newrelic oauth nuntium poirot guisso database aws log_file app_config step_config api recaptcha login hub telemetry).each do |file|
      run "ln -nfs #{shared_path}/config/#{file}.yml #{release_path}/config/"
    end

    verboice_config = YAML.load_file(File.expand_path('../verboice.yml', __FILE__))
    if ['yes', 'true', 'on'].include? verboice_config["plugins"]["report"]
      run "ln -nfs #{shared_path}/config/plugins/reports/settings.yml #{release_path}/plugins/reports/config/"
    end
  end

  task :symlink_data, :roles => :app do
    run "ln -nfs #{shared_path}/data #{release_path}/"
  end

  task :symlink_help, :roles => :app do
    run "ln -nfs #{shared_path}/help #{release_path}/public"
  end
  
  task :generate_version, :roles => :app do
    run "cd #{release_path} && git describe --always > #{release_path}/VERSION"
  end
end

namespace :verboice do
  desc 'Update Verboice Broker service Ubuntu systemd'
  task :update_broker, :roles => :app do
    service_name = 'verboice-broker.service'
    file_path = "#{shared_path}/systemd/#{service_name}"
    service_path = "/lib/systemd/system/#{service_name}"
    
    sudo "systemctl disable #{service_name}"
    sudo "cp --remove-destination #{file_path} #{service_path}"
    sudo 'systemctl daemon-reload'
    sudo "systemctl enable #{service_name}"
  end

  desc 'Update Verboice delayed job service Ubuntu systemd'
  task :update_delayed_job, :roles => :app do
    service_name = 'verboice-delayed-job.service'
    file_path = "#{shared_path}/systemd/#{service_name}"
    service_path = "/lib/systemd/system/#{service_name}"
    
    sudo "systemctl disable #{service_name}"
    sudo "cp --remove-destination #{file_path} #{service_path}"
    sudo 'systemctl daemon-reload'
    sudo "systemctl enable #{service_name}"
  end

  desc 'Quiet Verboice dependencies, broker and delayed job services'
  task :quiet, :roles => :app do
    run 'sudo pgrep -f verboice-broker | xargs kill -USR1'
    run 'sudo pgrep -f verboice-delayed_job | xargs kill -USR1'
  end

  desc 'Start Verboice dependencies, broker and delayed job services'
  task :start, :roles => :app do
    run 'sudo systemctl start verboice-broker.service'
    run 'sudo systemctl start verboice-delayed-job.service'
  end

  desc 'Stop Verboice dependencies, broker and delayed job services'
  task :stop, :roles => :app do
    run 'sudo systemctl stop verboice-broker.service'
    run 'sudo systemctl stop verboice-delayed-job.service'
  end

  desc "Restart Verboice dependencies, broker and delayed job services"
  task :restart, :roles => :app do
    sudo 'systemctl restart verboice-broker.service'
    sudo 'systemctl restart verboice-delayed-job.service'
  end
end

namespace :foreman do
  desc 'Export the Procfile to Ubuntu upstart scripts'
  task :export, :roles => :app do
    if ENV['RVM']
      run "echo -e \"HOME=$HOME\\nPATH=$PATH\\nGEM_HOME=$GEM_HOME\\nGEM_PATH=$GEM_PATH\\nRAILS_ENV=production\" >  #{current_path}/.env"
      run "cd #{current_path} && rvmsudo bundle exec foreman export upstart /etc/init -f #{current_path}/Procfile -a #{application} -u #{user} -t #{current_path}/etc/upstart --concurrency=\"broker=1,delayed=1\""
    else
      run "echo -e \"HOME=$HOME\\nPATH=$PATH\\nRAILS_ENV=production\" >  #{current_path}/.env"
      run "cd #{current_path} && #{try_sudo} `which bundle` exec foreman export upstart /etc/init -f #{current_path}/Procfile -a #{application} -u #{user} -t #{current_path}/etc/upstart --concurrency=\"broker=1,delayed=1\""
    end
  end

  desc "Start the application services"
  task :start, :roles => :app do
    sudo "start #{application}"
  end

  desc "Stop the application services"
  task :stop, :roles => :app do
    sudo "stop #{application}"
  end

  desc "Restart the application services"
  task :restart, :roles => :app do
    run "sudo start #{application} || sudo restart #{application}"
  end
end

before 'deploy:finalize_update', "deploy:symlink_configs"
before "deploy:start", "deploy:migrate"
before "deploy:restart", "deploy:migrate"
after "deploy:update_code", "deploy:symlink_configs"
after "deploy:update_code", "deploy:symlink_data"
after "deploy:update_code", "deploy:symlink_help"
after "deploy:update_code", "deploy:prepare_broker"
after "deploy:update_code", "deploy:compile_broker"

if ENV['UBUNTU'].to_f >= 16
  after 'deploy:update', 'verboice:update_broker'
  after 'deploy:update', 'verboice:update_delayed_job'
  after 'deploy:restart', 'verboice:restart'
else
  after "deploy:update", "foreman:export"    # Export foreman scripts
  after "deploy:restart", "foreman:restart"   # Restart application scripts
end
