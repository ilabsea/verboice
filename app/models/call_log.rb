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

class CallLog < ActiveRecord::Base
  include CallLogSearch

  CSV_MAX_ROWS = 262144 # 2 ^ 18
  
  STATE_ACTIVE = :active
  STATE_COMPLETED = :completed
  STATE_FAILED = :failed
  STATE_SUSPENDED = :suspended

  FAIL_REASONS = {
    'failed'    => 'failed',
    'error'     => 'failed',
    'timeout'   => 'no_answer',
    'no_answer' => 'no_answer',
    'busy'      => 'hangup',
    'hangup'    => 'incompleted',
    'marked_as_failed' => "marked_as_failed",
    'no_ack' => 'no_ack',
    'blocked' => "blocked",
    'disabled' => "disabled"
  }

  DATE_FORMAT_EXPORT = [
    ["D/M/Y", '%d/%m/%Y %H:%M:%S %z'],
    ["M/D/Y", '%m/%d/%Y %H:%M:%S %z'],
    ["Y/M/D", '%Y/%m/%d %H:%M:%S %z']
  ]

  belongs_to :account
  belongs_to :project
  belongs_to :contact
  belongs_to :call_flow
  belongs_to :channel
  belongs_to :schedule
  has_many :traces, :foreign_key => 'call_id'
  has_many :entries, :foreign_key => 'call_id', :class_name => "CallLogEntry"
  has_many :call_log_answers, :dependent => :destroy
  has_many :recorded_audios, :dependent => :destroy
  has_many :call_log_recorded_audios, :dependent => :destroy
  has_many :pbx_logs, :foreign_key => :guid, :primary_key => :pbx_logs_guid

  before_validation :set_account_to_project_account, :if => :call_flow_id?

  validates_presence_of :account
  validates_presence_of :project
  validates_presence_of :channel

  delegate :time_zone, to: :project, prefix: true, allow_nil: true

  belongs_to :parent, class_name: CallLog

  def state
    read_attribute(:state).try(:to_sym)
  end

  def direction
    read_attribute(:direction).try(:to_sym)
  end

  def outgoing?
    direction == :outgoing
  end

  def incoming?
    direction == :incoming
  end

  def start_incoming
    info "Answering call from #{address}"
    start
  end

  def start_outgoing(address)
    self.address = address
    info "Calling #{address}"
    start
  end

  def start
    self.state = :active
    self.started_at = Time.now.utc
    self.save!
  end

  def finish_with_error(message)
    self.fail_reason = message
    finish :failed
  end

  def address_without_prefix
    return address if prefix_called_number.nil? || address.nil?
    return address[prefix_called_number.size..-1] if address.start_with? prefix_called_number
  end

  def finish_successfully
    self.fail_reason = ''
    finish :completed
  end

  def finish(state)
    self.state = state
    self.finished_at = Time.now.utc
    self.save!

    begin
      call_flow.try(:push_results, self) if call_flow && call_flow.store_in_fusion_tables
    rescue Exception => ex
      logger.error "Error pushing call flow results #{ex.message}\n#{ex.backtrace.join("\n")}"
    end
  end

  def structured_details
    entries.inject [] do |logs, call_log_entry|
      logs << {:severity => call_log_entry.severity, :time => call_log_entry.created_at, :text => call_log_entry.description}
    end
  end

  def interaction_details
    trace = traces.build(step_name: 'end', created_at: finished_at) if finished_at.present?
    traces.inject([]) { |details, trace| details << trace.summary(created_at) }.tap do |interaction|
      traces.delete(trace) if trace.present?
    end
  end

  def calculate_duration
    (finished_at - (not_before.nil? ? started_at : not_before)).to_i
  end

  CallLogEntry::Levels.each do | severity |
    class_eval <<-EVAL, __FILE__, __LINE__ + 1
      def #{severity}(description, options = {})
        log :#{severity}, description, options
      end
    EVAL
  end

  def log(level, description, options = {})
    CallLogEntry.create!(
      severity: level,
      description: description,
      call_id: self.id,
      step_id: options[:step_id],
      step_name: options[:step_name],
      command: options[:command],
      action: options[:action]
    )
  end

  def last_entry
    self.entries.order('created_at DESC, id DESC').first
  end

  def self.audios_size
    logs = CallLog.find_by_sql scoped.joins(:call_log_recorded_audios).select('call_logs.id').group('call_logs.id').to_sql
    logs.inject(0) do |result, log|
      result += RecordingManager.for(log).size
    end
  end

  def self.by_account_id account_id
    where(account_id: account_id)
  end

  def self.by_channel_id channel_id
    where(channel_id: channel_id)
  end

  def self.between from, to
    where(started_at: from..to)
  end

  private

  def set_account_to_project_account
    self.project_id = self.call_flow.project_id
    self.account_id = self.project.account_id
    contact = self.project.contacts.joins(:addresses).where(:contact_addresses => {:address => self.address}).first
    self.contact_id = contact.id unless contact.nil?
  end
end
