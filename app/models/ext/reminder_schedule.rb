module Ext
	class ReminderSchedule < ExtActiveRecord
		include ActiveModel::Validations

 		DEFAULT_DATE_TIME_FORMAT  = '%Y-%m-%d %H:%M'

		serialize :queue_call_id
		serialize :conditions, Array
		serialize :schedule, IceCube::Schedule

		#TODO : alias attribute for :date_time
		validates :client_start_date, :"ext/validator/date" => {:date_format => Date::DEFAULT_FORMAT, :field => :start_date }, :if => Proc.new { |record| !record.repeat? }
		validates :conditions, :presence => true, :if => Proc.new { |record| record.repeat? }

		validates :time_from, :presence => true
		validates :time_to, :presence => true

		validate :time_from_is_before_time_to

		validates :retries_in_hours, :presence => true, :if => Proc.new { |record| record.retries }
		validates_format_of :retries_in_hours, :with => /^[0-9\.]+(,[0-9\.]+)*$/, :allow_blank => true

		validates :call_flow_id, :presence => true

		validates :reminder_group_id, :presence => true

		belongs_to :call_flow
		belongs_to :reminder_group
		belongs_to :reminder_phone_book_type
		belongs_to :channel

		belongs_to :retries_schedule, :class_name => "Schedule", :foreign_key => :retries_schedule_id
		belongs_to :project

		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule", :dependent => :destroy
		assign_has_many_to "CallFlow", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule", :dependent => :nullify


		has_many :reminder_channels, :class_name => "Ext::ReminderChannel", :inverse_of => :reminder_schedule, :dependent => :destroy
		has_many :channels, :through => :reminder_channels
		accepts_nested_attributes_for :reminder_channels

		TYPE_ONE_TIME = 0
		TYPE_DAILY   = 1

		attr_reader :start_date_display
		attr_accessor :client_start_date, :ext_reminder_channels_attributes

		before_save   :initialize_schedule_and_schedule_retries
		after_create  :create_queued_calls
		after_destroy :remove_queued_calls

		def time_from_is_before_time_to
			if client_start_date
				client_date = repeat? ? DateTime.now.utc.in_time_zone(project.time_zone).to_date : Ext::Parser::DateParser.parse("#{client_start_date}", Date::DEFAULT_FORMAT)

			  start_date_time = Ext::Parser::TimeParser.parse("#{client_date.to_string} #{time_from}", DateTime::DEFAULT_FORMAT_WITHOUT_TIMEZONE, project.time_zone)
			  end_date_time = Ext::Parser::TimeParser.parse("#{client_date.to_string} #{time_to}", DateTime::DEFAULT_FORMAT_WITHOUT_TIMEZONE, project.time_zone)
	      
	      errors[:base] << "End time must be greater than the start time." if start_date_time.greater_than? end_date_time
		  end
    end

		def initialize_schedule_and_schedule_retries
			# schedule type
			self.update_schedule_type!

			# create IceCube schedule recurrence
			self.create_schedule_recurrence!

			# schedule retries
			self.update_retries_schedule!
		end

		def create_with_channels params
			params.slice()

		end

		def create_queued_calls
			process reminder_group.addresses, DateTime.now.utc.in_time_zone(project.time_zone) if reminder_group and reminder_group.has_addresses?
		end

		def update_reminder_schedule_with_queues_call params
			if update_attributes(params)
			  update_queues_call
			  return true
			end
			false
		end

		def update_queues_call
			remove_queued_calls
			create_queued_calls
		end

		def remove_queued_calls
			if queue_call_id
				begin
					queued_calls = QueuedCall.find_all_by_id(queue_call_id)
					queued_calls.each do |queued_call|
						queued_call.call_log.destroy
						queued_call.destroy
					end
				rescue Exception => e	
					p e.message
				end

				self.queue_call_id = [] if self.persisted?
			end
		end

		def self.channel_migrate_reminder_schedule
     	   ReminderSchedule.includes(:channel).find_each do |reminder_schedule|
 	   	     next if !reminder_schedule.channel
     	   	 channel = reminder_schedule.channel
     	   	 reminder_channel = reminder_schedule.reminder_channels.build(channel_id: channel.id)
     	   	 reminder_channel.save
     	   end
		end

		def self.schedule project_id, at_time
			project = Project.find(project_id)
			project.ext_reminder_schedules.each do |reminder_schedule|
				addresses = reminder_schedule.reminder_group.addresses if reminder_schedule.reminder_group && reminder_schedule.reminder_group.has_addresses?
				addresses = [] if addresses.nil?
				reminder_schedule.process addresses, at_time.in_time_zone(project.time_zone), true unless addresses.empty?
			end
		end

		def process addresses, running_time, is_schedule = false
			if running_time.to_date.equal? start_date
				should_enqueue = true if (from_date_time.greater_or_equal?(running_time) || running_time.between?(from_date_time, to_date_time) || is_schedule) && in_schedule_date?(running_time.to_date)
			elsif running_time.to_date.greater_than?(start_date)
				if in_schedule_date? running_time.to_date
					should_enqueue = true if repeat?
				end
			end

			if should_enqueue
				phone_numbers = callers_matches_conditions addresses
				enqueued_call(phone_numbers, running_time) unless phone_numbers.empty?
			end
		end

		def call_options at_time
			call_time_string = "#{at_time.to_string(Date::DEFAULT_FORMAT)} #{time_from}"
			not_before = Ext::Parser::TimeParser.parse(call_time_string, DateTime::DEFAULT_FORMAT_WITHOUT_TIMEZONE, self.project.time_zone)
			not_before = not_before > Time.now ? not_before : Time.now
			
			options = {
				call_flow_id: self.call_flow_id,
				project_id: self.project_id,
				# time_zone: self.project.time_zone,
				not_before: not_before
			}
			options[:account] = self.project.account if self.project
			options[:schedule_id] = self.retries_schedule.id if self.retries_schedule
			options
		end

		def enqueued_call addresses, at_time
			if call_flow
				options = call_options at_time
				queues = []
				addresses.each do |address|
					suggested_channel = suggested_channel_for address
					if suggested_channel
						call_log = suggested_channel.call(address, options)
						raise call_log.fail_reason if call_log.fail_reason
						queue =  QueuedCall.find_by_call_log_id(call_log.id) 
						queues << queue.id if queue
					end
				end
				self.queue_call_id = queues
				self.save
				queues
			end
		end

		def suggested_channel_for address
	    self.channels.each do |channel|
	      suggestion = channel.config["prefix"]
	      return channel if ReminderSchedule.address_matched_suggest? address, suggestion
			end
			self.channels.first
		end

		def self.address_matched_suggest? address, suggestion
		   return false if !suggestion
		   fields = suggestion.split(Channel::PREFIX_SEPARATOR)

		   fields.each do |field|
		   	  return true if address.start_with?(field)
		   end
		   false
		end

		def repeat?
			self.schedule_type == ReminderSchedule::TYPE_DAILY
		end

		def in_schedule_date? date
			schedule.occurs_on? date if schedule
		end

		def create_start_date!
			unless repeat?
				self.start_date = Ext::Parser::DateParser.parse(self.client_start_date) if client_start_date
			else
				self.start_date = DateTime.now.utc.in_time_zone(project.time_zone).to_date if self.start_date.nil?
			end
		end

		def start_date_display
			start_date.to_string(Date::DEFAULT_FORMAT)
		end

		def client_start_date
			@client_start_date
		end

		def client_start_date=(val)
			@client_start_date = val
		end

		def has_conditions?
			exists = false
			exists = true if !conditions.nil? && !conditions.empty?
			exists
		end

		def callers_matches_conditions addresses
			phone_numbers = []
			if has_conditions?
				addresses.each do |address|
					contact = project.contacts.joins(:addresses).where(:contact_addresses => {:address => address}).last
					phone_numbers.push address if contact and contact.evaluate? conditions.first
				end
			else
				phone_numbers = addresses
			end
			phone_numbers
		end

		def create_schedule_recurrence!
			if repeat?
				rule = IceCube::Rule.daily
			end

			self.schedule = IceCube::Schedule.new(start = from_date_time, :duration => to_date_time.to_i - from_date_time.to_i)
			self.schedule.add_recurrence_rule rule if rule
		end

		def update_schedule_type!
			self.conditions = [] unless repeat?
			create_start_date!
		end

		def update_retries_schedule!
			if retries
				schedule_model = self.retries_schedule.nil? ? project.schedules.build : self.retries_schedule
				schedule_model.name = "reminder_schedule_retries_#{Guid.new.to_s}"
				schedule_model.retries = self.retries_in_hours
			 	schedule_model.time_from = from_date_time.to_time
			 	schedule_model.time_to = to_date_time.to_time
			 	schedule_model.disabled = true # disalbe from schedule list UI
			 	schedule_model.weekdays = Ext::Weekday::DAY_NAMES.map { |x| Ext::Weekday::DAY_NAMES.index(x) }.join(",")

				self.retries_schedule = schedule_model if schedule_model.save
			else
				self.retries_in_hours = nil
				# remove retries schedule references
				self.retries_schedule.destroy if self.retries_schedule
			end
		end

		def from_date_time
			date_time_string = "#{start_date.to_string(Date::DEFAULT_FORMAT)} #{time_from}"
			Ext::Parser::TimeParser.parse(date_time_string, DateTime::DEFAULT_FORMAT_WITHOUT_TIMEZONE, project.time_zone)
		end

		def to_date_time
			date_time_string = "#{start_date.to_string(Date::DEFAULT_FORMAT)} #{time_to}"
			Ext::Parser::TimeParser.parse(date_time_string, DateTime::DEFAULT_FORMAT_WITHOUT_TIMEZONE, project.time_zone)
		end

		def reset_repeat_everyday_to_one_time!
			if self.repeat? && self.conditions.empty?
				self.client_start_date = self.start_date.to_string
				self.schedule_type = TYPE_ONE_TIME
				self.save
			end
		end

		def reset_one_time_with_condition_to_condition!
			if !self.repeat? && !self.conditions.empty?
				self.schedule_type = TYPE_DAILY
				self.save
			end
		end

		def reset_repeat_on_specific_day_with_condition_to_condition!
			if self.repeat? && !self.conditions.empty?
				self.save
			end
		end

	end

end
