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
  class Compressor
    def initialize file_name
      @file_name = file_name
    end

    def extract_to! path
      Log.info(:s3_log_dir, "extracting compress file to #{path}")
      system "tar -xf #{@file_name} -C #{path}"
    end

    def remove_extract_from! path
      Log.info(:s3_log_dir, "removing extract files")
      extraction_path = File.join(path, Backup::TEMP_DIR) # extraction path
      FileUtils.rm_rf extraction_path if File.exists? extraction_path
    end
  end
end