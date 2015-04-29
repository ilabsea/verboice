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

module Amazon
  class S3
    CONFIG_FILE_PATH = "#{Rails.root}/config/aws.yml"

    def initialize
      @s3 = AWS::S3.new
    end

    class << self
      def upload file
        unless validate?(file)
          Log.info(:s3_log_dir, "error at #{Time.now.to_s}, file can't be null")
          raise "file can't be null"
        end

        instance = Amazon::S3.new
        instance.upload file
      end

      def restore year, month, type
        instance = Amazon::S3.new
        instance.restore year, month, type
      end

      def validate? file
        file && File.exists?(file) ? true : false
      end
    end

    class PartFile
      PART_FILE_SIZE_MB = 10

      def initialize file
        @file = file
      end

      def part_file_prefix
        File.join(Backup::PART_FILE_DIR, "#{File.basename(@file)}_")
      end

      def get_files
        Dir.glob("#{part_file_prefix}*")
      end

      def split!
        system("split -b #{PART_FILE_SIZE_MB}M #{@file} #{part_file_prefix}")
      end

      def clear!
        FileUtils.rm_rf Backup::PART_FILE_DIR
      end
    end

    def bucket
      return nil unless File.exists?(CONFIG_FILE_PATH)
      aws = YAML::load(File.open(CONFIG_FILE_PATH))[Rails.env]
      @bucket ||= @s3.buckets[aws['bucket_name']]
    end

    def object(file)
      if file
        @key = File.basename(file)
        @object ||= bucket.nil? ? nil : bucket.objects[@key]
      end
    end

    def upload original_file
      part_file = PartFile.new original_file
      part_file.split!

      Log.info(:s3_log_dir, "uploading to amazon s3")
      object(original_file).multipart_upload do |multipart_upload|
        part_file.get_files.sort!.each do |file|
          Log.info(:s3_log_dir, "uploading #{file} to amazon s3")
          multipart_upload.add_part  file: file, content_length: File.size(file)
          Log.info(:s3_log_dir, "uploaded #{file} to amazon s3")
        end
        multipart_upload.complete(:remote_parts)
      end
      Log.info(:s3_log_dir, "done")

      part_file.clear!
    end

    def restore year, month, type
      Log.info(:s3_log_dir, "restore: retrieving objects from amazon s3")
      @objects = []
      pattern = "^"
      pattern << Regexp.escape("#{year}") << Regexp.escape("#{'%02d' % month}")
      pattern << ".*"
      pattern << Regexp.escape("#{type}.tar.gz") << "$"
      regex = Regexp.new pattern
      if bucket
        bucket.objects.each do |obj|
          if obj.key.match regex
            @objects.push obj
            break if type == Backup::FULL
          end
        end
      end
      @objects
    end
  end
end