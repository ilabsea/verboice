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

module CallLogHelper
  def render_csv_download? project_id, search = nil
    logs = CallLog.where project_id: project_id
    logs = logs.search search
    audios = CallLogRecordedAudio.where call_log_id: logs 

    logs.count > CallLog::CSV_MAX_ROWS || audios.count == 0
  end

  def datetime_format_csv(datetime, time_zone, format)
    datetime = datetime.try(:in_time_zone, time_zone || 'UTC')
    datetime.present? ? datetime.strftime(format) : ''
  end
end
