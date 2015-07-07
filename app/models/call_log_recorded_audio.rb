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

class CallLogRecordedAudio < ActiveRecord::Base
  belongs_to :call_log
  belongs_to :project_variable

  attr_accessible :description, :annotation, :key, :call_log_id, :project_variable_id

  validates_presence_of :call_log_id
  validates_presence_of :project_variable_id

  validates_uniqueness_of :project_variable_id, :scope => :call_log_id
end
