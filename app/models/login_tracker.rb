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

class LoginTracker < ActiveRecord::Base
  attr_accessible :email, :origin_ip, :status, :marked_as, :logged_in_at

  STATUS_ACTIVE = 'ACTIVE'
  STATUS_INACTIVE = 'INACTIVE'

  MARKED_AS_FAILED = 'FAILED'
  MARKED_AS_SUCCESS = 'SUCCESS'

  validates :email, :origin_ip, :status, :marked_as, :logged_in_at, presence: true
  before_create :disable_previous_failed_history

  class << self
    def log ip, email, marked_as
      LoginTracker.create!(email: email, origin_ip: ip, logged_in_at: Time.now, status: LoginTracker::STATUS_ACTIVE, marked_as: marked_as)
    end
  end

  def disable_previous_failed_history
    LoginTracker.where(origin_ip: origin_ip, email: email).last_failed.each do |tracker|
      tracker.status = STATUS_INACTIVE
      tracker.save
    end if success?
  end

  def self.active
    where(status: STATUS_ACTIVE)
  end

  def self.failed
    where(marked_as: MARKED_AS_FAILED)
  end

  def self.last_duration_failed_attempt
    where("logged_in_at >= ?", LoginSetting.duration_failed_attempt.minutes.ago)
  end

  def self.last_failed
    active.failed.last_duration_failed_attempt
  end

  private

  def success?
    marked_as == MARKED_AS_SUCCESS
  end

end
