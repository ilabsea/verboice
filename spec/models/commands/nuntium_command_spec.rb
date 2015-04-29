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

require 'spec_helper'

describe Commands::NuntiumCommand do
  it 'should serialize parameters with channel_id if present' do
    command = Commands::NuntiumCommand.new('resource_guid', 7, :caller)
    command.serialize_parameters.should eq({
      expr: nil,
      resource_guid: 'resource_guid',
      rcpt_type: :caller,
      channel_id: 7
    })
  end

  it 'should serialize parameters without channel_id if channel is missing' do
    command = Commands::NuntiumCommand.new('resource_guid', nil, :caller)
    command.serialize_parameters.should eq({
      expr: nil,
      resource_guid: 'resource_guid',
      rcpt_type: :caller
    })
  end
end
