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

class Restore

  BASEDIR = 'tmp/restore'

  def initialize
    prepare!
  end

  class << self
    def method_missing(method, *args)
      cmd_class = "Restores::#{method.to_s.camelize}".constantize
      cmd_class.new *args
    end
  end

  def db_config
    @db_config ||= Rails.configuration.database_configuration[Rails.env]
  end

  def current_dir
    @current_dir ||= File.join(BASEDIR, "#{self.type}")
  end

  def insert_file_name file_name
    @file_names ||= []
    @file_names.push file_name
  end

  def prepare!
    Log.info(:s3_log_dir, "restore: preparing")
    FileUtils.mkdir BASEDIR unless File.exists? BASEDIR
    FileUtils.rm_rf current_dir if File.exists? current_dir
    FileUtils.mkdir current_dir
  end

  def restore! year, month
    download! year, month
    process!
  end

  def download! year, month
    Log.info(:s3_log_dir, "restore: streaming object")
    objects = Amazon::S3.restore year, month, self.type

    objects.each do |object|
      file_name = File.join(@current_dir, object.key)
      File.open(file_name, 'wb') do |file|
        object.read do |chunk|
         file.write(chunk)
        end
      end
      insert_file_name(file_name)
    end
  end

  def process!
    @file_names.each do |file_name|
      if File.exists? file_name
        compressor = Restores::Compressor.new file_name
        compressor.extract_to! current_dir
        restore_mysql_and_files!
        compressor.remove_extract_from!(current_dir)
      end
    end
  end

  def restore_mysql_and_files!
    backup_dir = File.join(current_dir, Backup::BASEDIR)
    if File.exists?(backup_dir) && File.directory?(backup_dir)
      Dir.foreach(backup_dir) do |d|
        next if d == "." or d == ".."
        dir = File.join(backup_dir, d)

        Log.info(:s3_log_dir, "restore: restoring files")
        if File.directory?(dir)
          # restoring files
          Restores::Directory.new(dir).restore!

          #restoring mysql
          directory = Restores::Directory.new(dir)
          sql_files = directory.lookup_files [".sql", Backup::BIN_LOGS.split("/").last]
          sql_files.each do |sql|
            mysql sql
          end
        end
      end
    end
  end

end