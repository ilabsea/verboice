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

class CustomTrafficSerializer < ActiveModel::Serializer
  attributes :address, :total_duration, :direction, :channel_id, :account_id

  def address
    return address = object.address if object.prefix_called_number.nil? || object.address.nil?
    return address = object.address[object.prefix_called_number.size..-1] if object.address.start_with? object.prefix_called_number
  end

end