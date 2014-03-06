#!/bin/bash

# Usage:
# ====================================
#
# bash verboice.instedd.org.bash IP_ADDRESS
# reload RVM
source /etc/profile.d/rvm.sh
if ! which rvm > /dev/null; then
  echo "===================================="
  echo "Before install application"
  echo "You have to install RVM first"
  echo "===================================="
  
  exit
fi

if ! which ruby > /dev/null; then
  echo "===================================="
  echo "Before install application"
  echo "You have to install ruby version 1.9.3 to RVM first"
  echo "===================================="
  
  exit
fi

if ! which passenger > /dev/null; then
  echo "===================================="
  echo "Before install application"
  echo "You have to install gem passenger to RVM first"
  echo "===================================="
  
  exit
fi

if [ $# -lt 1 ]; then
  echo "===================================="
  echo "Usage:"
  echo 'bash verboice.instedd.org.bash IP_ADDRESS'
  echo "===================================="
  
  exit
fi

# Capistrano deploy user
cap_deploy_user=ilab
hosts=$1
app_name=verboice
cap_deploy_directory=/u/apps/$app_name
cap_deploy_current=/u/apps/$app_name/current

# Ruby version
ruby_version=1.9.3

# Mysql root user setting
mysql_root_password=
echo "mysql-server mysql-server/root_password select $mysql_root_password" | debconf-set-selections
echo "mysql-server mysql-server/root_password_again select $mysql_root_password" | debconf-set-selections

# Packages
apt-get -y update
apt-get -y install build-essential curl mercurial lame sox festival asterisk mysql-server libmysqlclient-dev apache2 apache2-prefork-dev libapr1-dev libaprutil1-dev apache2-mpm-prefork libcurl4-openssl-dev postfix
apt-get -y upgrade

# create blank database in mysql for verboice production database
mysql -u root --password=$mysql_root_password -e "create database if not exists $app_name;"

# Deploy application
rails_root=/home/$cap_deploy_user/$app_name
if [ ! -d $rails_root ]; then
  repo=http://bitbucket.org/kakada/verboice/
  hg clone $repo $rails_root
else
  cd $rails_root
  hg update -C
  hg pull -u
fi

# allow normal switch to super user without typing password
# using for capistarno deployment
echo "allow normal user switch to super user without typing password"
ilab_user_conf=/etc/sudoers.d/$cap_deploy_user
if [ ! -f $ilab_user_conf ]; then
  echo "$cap_deploy_user ALL=(ALL) NOPASSWD:ALL" >> $ilab_user_conf
  chmod 0440 $ilab_user_conf
fi

if [ -d $rails_root ]; then
  # Bundle install Gemfile
  cd $rails_root
  # reload RVM
  source /etc/profile.d/rvm.sh
  rvm use $ruby_version@$app_name

  # install gem to gemset $app_name
  bundle install

  # Set up capistrano deploy directories
  cap deploy:setup HOSTS=$hosts
fi

# Copy neccessary configuration files to capistrano shared directory
if [ -d $cap_deploy_directory/shared ]; then
  cp $rails_root/config/*.yml $cap_deploy_directory/shared/
  cp $rails_root/config/*.yml.template $cap_deploy_directory/shared/
  [ ! -d $cap_deploy_directory/shared/data ] && mkdir $cap_deploy_directory/shared/data
fi

# Change capistrano deploy directories owner to ilab
if [ -d $cap_deploy_directory ]; then
  chown -R $cap_deploy_user:$cap_deploy_user $cap_deploy_directory
fi

# deploy application via capistrano
if [ -d $rails_root ]; then
  # Bundle install Gemfile
  cd $rails_root
  # reload RVM
  source /etc/profile.d/rvm.sh
  rvm use $ruby_version@$app_name
  # install gem to gemset $app_name
  bundle install
  # Set up capistrano deploy directories
  cap deploy HOSTS=$hosts
fi

# Configure asterisk with Verboice application
if which asterisk > /dev/null; then
  if [ -d /etc/asterisk ]; then
    sip_verboice_channels_conf=/etc/asterisk/sip_verboice_channels.conf
    sip_verboice_registrations_conf=/etc/asterisk/sip_verboice_registrations.conf

    # replace asterisk configuration in /etc/asterisk with $rails_root/etc/asterisk
    if [ ! -f $sip_verboice_channels_conf ]; then
      if [ ! -f $sip_verboice_registrations_conf ]; then
        if [ -d $rails_root ]; then
          rm -fr /etc/asterisk/*
          cp $rails_root/etc/asterisk/* /etc/asterisk/
        fi
      fi
    fi

    # create sip_verboice_channels.conf allow other write permission to /etc/asterisk
    if [ ! -f $sip_verboice_channels_conf ]; then
      touch $sip_verboice_channels_conf
      chmod o+w $sip_verboice_channels_conf
    fi
    # create sip_verboice_registrations.conf allow other write permission to /etc/asterisk
    if [ ! -f $sip_verboice_registrations_conf ]; then
      touch $sip_verboice_registrations_conf
      chmod o+w $sip_verboice_registrations_conf
    fi

    # restart Asterisk
    /etc/init.d/asterisk restart
  fi
fi

# Set up verboice directory to store sounds recording
verboice_sounds_recording_directory=/usr/share/asterisk/sounds/$app_name
if [ ! -d $verboice_sounds_recording_directory ]; then
  mkdir $verboice_sounds_recording_directory
  chmod o+w $verboice_sounds_recording_directory
fi

# Configure Rails application on apache
default_host_conf=/etc/apache2/sites-available/default
if [ -f $default_host_conf ]; then
  # Modify default host of apache2
  echo "<VirtualHost *:80>" > $default_host_conf
  echo "DocumentRoot $cap_deploy_current/public" >> $default_host_conf
  echo "PassengerSpawnMethod conservative" >> $default_host_conf
  echo "</VirtualHost>" >> $default_host_conf

  # Restart web server
  /etc/init.d/apache2 restart
fi