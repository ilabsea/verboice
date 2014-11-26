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

class Commands::NuntiumCommand < Command

  def initialize(kind, options = {})
    @kind = kind
    @rcpt_type = options[:rcpt_type] || nil
    @expr = options[:expr] || nil
    @subject_guid = options[:subject_guid] || nil
    @resource_guid = options[:resource_guid]
  end

  def serialize_parameters
    {
      kind: @kind,
      rcpt_type: @rcpt_type,
      expr: @expr,
      subject_guid: @subject_guid,
      resource_guid: @resource_guid
    }
  end

end
