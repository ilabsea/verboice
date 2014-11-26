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

class Backup

  TEMP_DIR = 'tmp/'
  PART_FILE_DIR = File.join(TEMP_DIR, "s3parts")
  BASEDIR = File.join(TEMP_DIR, "backups")
  BIN_LOGS = '/var/log/mysql/mysql-bin.[0-9]*'
  FULL = :full
  INCREMENTAL = :incremental

  attr_reader :name, :directory

  def initialize(name)
    @name = name
  end

  def db_config
    @db_config ||= Rails.configuration.database_configuration[Rails.env]
  end

  def asterisk_config
    @asterisk_config ||= YAML::load(File.read 'config/asterisk.yml')
  end

  def current_dir
    @current_dir ||= [BASEDIR, '/', @name].join
  end

  def file_compression
    @file_compression ||= "#{@directory[:current]}.tar.gz"
  end

  class << self
    def restore!
      now = Time.now
      Log.info(:s3_log_dir, "====== restore: started at #{Time.now.to_s} ======")
      Restore.full.restore!(now.year, now.month)
      Restore.incremental.restore!(now.year, now.month)
      Log.info(:s3_log_dir, "====== restore: finished at #{Time.now.to_s} ======")
    end

    def full!
      if File.exists? Amazon::S3::CONFIG_FILE_PATH
        Log.info(:s3_log_dir, "====== full backup: started at #{Time.now.to_s} ======")
        backup = setup Backup::FULL
        backup.copy_files
        backup.mysqldump
        backup.compress
        
        # uploading to aws
        Amazon::S3.upload backup.file_compression

        backup.remove_files
        Log.info(:s3_log_dir, "====== full backup: finished at #{Time.now.to_s} ======")
      end
    end

    def incremental!
      if File.exists? Amazon::S3::CONFIG_FILE_PATH
        Log.info(:s3_log_dir, "====== incremental backup: started at #{Time.now.to_s} ======")
        backup = setup Backup::INCREMENTAL
        backup.copy_files
        backup.incremental
        backup.compress
        
        # uploading to aws
        Amazon::S3.upload backup.file_compression

        backup.remove_files
        Log.info(:s3_log_dir, "====== incremental backup: finished at #{Time.now.to_s} ======")
      end
    end

    def setup type
      instance = Backup.new "#{Time.now.strftime '%Y%m%d%H%M%S'}_#{type}"
      instance.prepare!
      instance
    end

  end

  def prepare!
    Log.info(:s3_log_dir, "backup: preparing")
    @directory = {
      current: current_dir,
      config: [current_dir, '/', 'config'].join,
      asterisk: [current_dir, '/', 'asterisk'].join,
      asterisk_etc: [current_dir, '/', 'asterisk', '/', 'etc'].join,
      asterisk_sounds: [current_dir, '/', 'asterisk', '/', 'sounds'].join
    }
    FileUtils.mkdir TEMP_DIR unless File.exists? TEMP_DIR
    FileUtils.mkdir PART_FILE_DIR unless File.exists? PART_FILE_DIR
    FileUtils.mkdir BASEDIR unless File.exists? BASEDIR
    FileUtils.mkdir @directory.values
  end

  def copy_files
    Log.info(:s3_log_dir, "backup: copying")
    system "cp -rH data #{@directory[:current]}"
    system "cp config/*.yml #{@directory[:config]}"
    # asterisk files
    system "cp #{asterisk_config['config_dir']}/* #{@directory[:asterisk_etc]}"
    system "cp -rH #{asterisk_config['sounds_dir']}/verboice #{@directory[:asterisk_sounds]}"
  end

  def mysqldump
    Log.info(:s3_log_dir, "backup: mysqldump database")
    cmd = "mysqldump --single-transaction -u#{db_config['username']} --flush-logs"
    cmd << " -p'#{db_config['password']}'" if db_config['password'].present?
    cmd << " #{db_config['database']} > #{@directory[:current]}/verboice.sql"
    system(cmd)
  end

  def incremental
    Log.info(:s3_log_dir, "backup: copying mysql log_bin")
    execute_sql 'flush logs'
    # backup binary logs
    logs = Dir.glob(BIN_LOGS).sort
    execute_sql "purge master logs to '#{File.basename(logs.pop)}'" do
      logs.each do |log|
        system "cp #{log} #{@directory[:current]}"
      end
    end
  end

  def compress
    Log.info(:s3_log_dir, "backup: compressing")
    system "tar -zcf #{file_compression} #{@directory[:current]}"
    # clean up
    FileUtils.rm_rf @directory[:current]
  end

  def remove_files
    FileUtils.rm_rf file_compression
  end

  private
  def execute_sql sql
    Log.info(:s3_log_dir, "backup: executing mysql command")
    yield if block_given?
    cmd = %{mysql -u#{db_config['username']} -e "#{sql}"}
    cmd << " -p'#{db_config['password']}'" if db_config['password'].present?
    system(cmd)
  end
end