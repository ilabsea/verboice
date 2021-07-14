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

class ReminderGroupSerializer < ActiveModel::Serializer
  attributes :id, :name, :addresses, :created_at, :updated_at, :sync_status_updated_at,
             :mode, :enable_sync, :sync_config, :reminder_group_contacts

  def addresses
    object.reminder_group_contacts.pluck(:address)
  end

  def updated_at
    object.updated_at.in_time_zone(object.project.time_zone)
  end

  def sync_status_updated_at
    object.sync_status_updated_at.in_time_zone(object.project.time_zone)
  end
end
