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

class QueuedCall < ActiveRecord::Base
  belongs_to :channel
  belongs_to :call_log
  belongs_to :schedule
  belongs_to :project
  belongs_to :call_flow

  serialize :flow, Command::BrokerFlow
  serialize :variables, Hash
  serialize :callback_params, Hash

  STATE_QUEUED = "queued"
  STATE_PAUSED = "paused"

  class << self
    def pause ids = []
      ids.each do |id|
        queued_call = QueuedCall.find(id)
        queued_call.pause!
      end
    end

    def resume ids = []
      ids.each do |id|
        queued_call = QueuedCall.find(id)
        if queued_call.paused?
          queued_call.resume!
        end
      end
    end
  end

  def start
    call_log.start_outgoing address
    new_session
  end

  def new_session
    options = {:call_log => call_log, :address => address}

    if call_flow.present?
      options[:call_flow] = call_flow
    elsif callback_url.present?
      options[:call_flow] = CallFlow.new :callback_url => callback_url, mode: :callback_url
    elsif flow.present?
      options[:call_flow] = CallFlow.new :flow => flow
    end

    if status_callback_url.present?
      options[:status_callback_url] = status_callback_url
    end

    options[:call_variables] = variables if variables
    options[:callback_params] = callback_params

    channel.new_session(options).tap do |session|
      session.queued_call = self
    end
  end

  def cancel_call!
    call_log.state = :cancelled
    call_log.save!
  end

  def notify_broker
    if channel
      begin
        channel.notify_broker
      rescue Exception => ex
        Rails.logger.info "Error notifying queued call #{id}: #{ex}"
      end
    else
      destroy
    end
  end

  def has_retries_left?
    schedule && schedule.retry_delays.count > retries
  end

  def next_retry_time
    sleep = schedule.retry_delays[retries - 1].to_f * (Rails.env == 'development' ? 1.second : 1.hour)

    schedule.with_time_zone(time_zone) do |time_zoned_schedule|
      time_zoned_schedule.next_available_time(Time.now.utc + sleep)
    end
  end

  def pause!
    self.state = STATE_PAUSED
    self.save
  end

  def resume!
    if self.past?
      # clone call_log
      old_call_log = self.call_log
      new_call_log = CallLog.new(old_call_log.attributes)
      new_call_log.save!

      # clone queued_call
      new_queued_call = QueuedCall.new(self.attributes.merge({call_log_id: new_call_log.id, state: STATE_QUEUED}))
      new_queued_call.save
      if should_trigger?
        new_queued_call.channel.notify_call_queued new_queued_call if new_queued_call.channel
      end

      # destroy itself paused one
      self.destroy
      old_call_log.destroy
    else
      self.state = STATE_QUEUED
      self.save
    end
  end

  def paused?
    self.state == STATE_PAUSED
  end

  def queued?
    self.state == STATE_QUEUED
  end

  def past?
    return true if !not_before? || (not_before? && self.not_before.less_or_equal?(DateTime.now))
  end

  def should_trigger?
    trigger = false
    trigger = true if past? && queued?
    trigger
  end

end
