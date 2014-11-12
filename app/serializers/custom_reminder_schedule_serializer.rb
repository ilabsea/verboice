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

class CustomReminderScheduleSerializer < ActiveModel::Serializer
  attributes :id, :project_id, :call_flow_id,
             :client_start_date, :time_from, :time_to, :conditions, :retries_in_hours,
             :reminder_group_id, :schedule_type

  has_many :channels, serializer: CustomBasicChannelSerializer

  def client_start_date
    self.object.start_date
  end

end