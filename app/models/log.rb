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

class Log
  CONFIG_FILE = "#{Rails.root}/config/log_file.yml"

  def initialize key
    if Log.config
      Dir.mkdir Log.config['log_dir'] unless File.exists?(Log.config['log_dir'])
      Dir.mkdir Log.config[key.to_s] unless File.exists?(Log.config[key.to_s])
      @file_name = File.join(Log.config[key.to_s], Date.today.to_s)
    end
  end

  class << self
    def info key, content
      instance = Log.new(key)
      instance.info(content)
    end

    def config
      return nil unless File.exists?(CONFIG_FILE)
      @config ||= YAML::load(File.open(CONFIG_FILE))
    end
  end

  def info content
    File.open @file_name, "a" do |f|
      f.puts content
    end if @file_name
  end
end
