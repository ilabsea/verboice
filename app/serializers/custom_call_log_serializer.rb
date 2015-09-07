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

class CustomCallLogSerializer < ActiveModel::Serializer
  attributes :id, :prefix_called_number, :address, :duration, :direction, :started_at,
             :call_flow_id, :state, :fail_reason, :not_before, :finished_at, :called_at

  has_one :account, serializer: CustomAccountSerializer
  has_one :project, serializer: CustomProjectSerializer
  has_one :channel, serializer: CustomChannelSerializer

  has_many :call_log_recorded_audios, serializer: CustomCallLogRecordedAudioSerializer
  has_many :call_log_answers, serializer: CustomSimpleCallLogAnswerSerializer


  def called_at
    return self.not_before if self.direction == 'outgoing' && self.not_before
    return self.started_at
  end
end
