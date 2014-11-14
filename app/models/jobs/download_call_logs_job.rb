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

require 'csv'

class Jobs::DownloadCallLogsJob < Struct.new(:account_id, :project_id, :search, :date_format)
  include ActionView::Helpers
  include ApplicationHelper

  CSV_TEMPLATE = 'app/views/call_logs/download_project_call_logs.csv.csvbuilder'

  def perform
    load_dependencies
    Zip::ZipOutputStream.open(@path) do |zos|
      zos.put_next_entry 'call_logs.csv'
      zos.print generate_csv

      # recorded audios
      @recorded_audios.each do |audio|
        zos.put_next_entry "audios/#{audio.call_log_id}_#{audio.key}.wav"
        zos.print IO.read "data/call_logs/#{audio.call_log_id}/results/#{audio.key}.wav"
      end
    end
  end

  def success job
    UserMailer.delay.generating_zip @project, @filename
  end

  def load_dependencies
    @account = Account.find account_id
    @filename = "call_logs_#{timestamp}.zip"
    @path = File.join RecordingManager.for(@account).path_for('downloads'), @filename

    @project = Project.includes(:project_variables).find project_id
    @logs = CallLog.where(project_id: @project).search search
    @recorded_audios = CallLogRecordedAudio.where call_log_id: @logs.pluck(:id)
    @date_format = date_format

    # csv options
    @input_encoding = 'UTF-8'
    @output_encoding = 'UTF-8'
    @csv_options = { :col_sep => ',' }

    # custom_audio_path
    @custom_audio_path = true
  end

  def timestamp
    @time ||= Time.now.strftime '%Y%m%d%H%M%S'
  end

  def generate_csv
    output = CsvBuilder::CSV_LIB.generate(@csv_options) do |faster_csv|
      csv = CsvBuilder::TransliteratingFilter.new faster_csv, @input_encoding, @output_encoding
      eval IO.read CSV_TEMPLATE
    end
  end
end
