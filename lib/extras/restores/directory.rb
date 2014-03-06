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

module Restores
  class Directory
    def initialize dir
      @dir = dir
    end

    def asterisk_config
      @asterisk_config ||= YAML::load(File.read 'config/asterisk.yml')
    end

    def restore!
      data_dir = File.join(@dir, "data")
      asterisk_sounds_dir = File.join(@dir, "asterisk", "sounds", "verboice")
      asterisk_etc_dir = File.join(@dir, "asterisk", "etc")
      config_dir = File.join(@dir, "config")

      system "cp -rH #{data_dir} #{Rails.root}"
      system "cp -rH #{config_dir} #{Rails.root}"
      # asterisk files
      unless File.exists?(asterisk_config['config_dir']) && File.directory?(asterisk_config['config_dir'])
        system "sudo mkdir #{asterisk_config['config_dir']}"
      end
      system "sudo cp #{asterisk_etc_dir}/* #{asterisk_config['config_dir']}"
      # change owner of sip_verboice_channels.conf & sip_verboice_registrations.conf to ilab:ilab
      sip_verboice_files = File.join(asterisk_config['config_dir'], "sip_verboice_*")
      system "sudo chmod o+w #{sip_verboice_files}"

      unless File.exists?(asterisk_config['sounds_dir']) && File.directory?(asterisk_config['sounds_dir'])
        system "sudo mkdir #{asterisk_config['sounds_dir']}"
      end

      verboice_sounds_dir = File.join(asterisk_config['sounds_dir'], "verboice")
      unless File.exists?(verboice_sounds_dir) && File.directory?(verboice_sounds_dir)
        system "sudo mkdir #{verboice_sounds_dir}"
      end
      system "sudo chmod o+w #{verboice_sounds_dir}"
      system "cp -rH #{asterisk_sounds_dir}/* #{verboice_sounds_dir}"
    end

    def lookup_files patterns
      sql_files = []
      Dir.foreach(@dir) do |file|
        if patterns.each.any? { |x| file.match x }
          sql_file = File.join(@dir, file)
          sql_files.push sql_file
        end
      end
      sql_files
    end
  end
end