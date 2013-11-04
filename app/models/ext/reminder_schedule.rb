module Ext
	class ReminderSchedule < ExtActiveRecord
		include ActiveModel::Validations

		DEFAULT_DATE_FORMAT  = '%Y-%m-%d'
 		DEFAULT_DATE_TIME_FORMAT  = '%Y-%m-%d %H:%M'

		serialize :queue_call_id
		serialize :conditions, Array
		serialize :schedule, IceCube::Schedule

		#TODO : alias attribute for :date_time
		validates :client_start_date, :"ext/validator/date" => {:date_format => Ext::ReminderSchedule::DEFAULT_DATE_FORMAT, :field => :start_date }

		validates :time_from, :presence => true
		validates :time_to, :presence => true

		validate :time_from_is_before_time_to

		validates_format_of :retries_in_hours, :with => /^[0-9\.]+(,[0-9\.]+)*$/, :allow_blank => true

		validates :call_flow_id, :presence => true
		validates :channel_id, :presence => true
		validates :reminder_group_id, :presence => true
		validates :days, :presence => true , :if => Proc.new {|record| record.repeat? }
		validates :recursion, :presence => true , :if => Proc.new {|record| record.repeat? }

		belongs_to :call_flow
		belongs_to :channel
		belongs_to :reminder_group
		belongs_to :reminder_phone_book_type

		belongs_to :retries_schedule, :class_name => "Schedule", :foreign_key => :retries_schedule_id
		belongs_to :project
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"

		TYPE_ONE_TIME = 0
		TYPE_DAILY   = 1
		# TYPE_WEEKLY  = 2

		attr_accessor :client_start_date

		before_save   :initialize_schedule_and_schedule_retries
		after_create  :create_queues_call
		after_destroy :remove_queues

		def time_from_is_before_time_to
			if client_start_date
				start_date_time = Ext::Parser::TimeParser.parse("#{client_start_date} #{time_from}", ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, project.time_zone)
				end_date_time = Ext::Parser::TimeParser.parse("#{client_start_date} #{time_to}", ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, project.time_zone)
		    errors[:base] << "End time must be greater than the start time." if start_date_time.greater_than? end_date_time
		  end
	  end

		def initialize_schedule_and_schedule_retries
			# create schedule
			create_start_date! unless client_start_date.nil?

			# create IceCube schedule recurrence
			self.create_schedule_recurrence!

			# schedule retries
			self.update_retries_schedule!
		end

		def create_queues_call
			process reminder_group.addresses, DateTime.now.utc.in_time_zone(project.time_zone) if reminder_group and reminder_group.has_addresses?
		end

		def update_queues_call
			remove_queue_call
			create_queues_call
		end

		def remove_queues
			begin
				queues = QueuedCall.find queue_call_id
				queues.each do |queue|
					queue.destroy
				end
			rescue Exception => e	
				p e.message
			end
		end

		def remove_queue_call
			if queue_call_id
				remove_queues
				self.queue_call_id = []
				self.save
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
			if in_schedule_date? running_time.to_date
				if running_time.to_date.equal? start_date
					should_enqueue = true if from_date_time.greater_or_equal?(running_time) || running_time.between?(from_date_time, to_date_time) || is_schedule
				elsif running_time.to_date.greater_than?(start_date)
					should_enqueue = true if schedule_type == ReminderSchedule::TYPE_DAILY
				end

				if should_enqueue
					phone_numbers = callers_matches_conditions addresses
					enqueued_call(phone_numbers, running_time) unless phone_numbers.empty?
				end
			end
		end

		def call_options at_time
			call_time_string = "#{at_time.to_string(Date::DEFAULT_FORMAT)} #{time_from}"

			not_before = Ext::Parser::TimeParser.parse(call_time_string, ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, self.project.time_zone)

			options = { 
				:call_flow_id => self.call_flow_id,
				:project_id => self.project_id,
				# :time_zone => self.project.time_zone,
				:not_before => not_before.utc
			}

			options[:schedule_id] = self.retries_schedule.id if self.retries_schedule
			options
		end

		def enqueued_call addresses, at_time
			options = call_options at_time
			queues = []
			addresses.each do |address|
				call_log = self.channel.call(address, options)
				raise call_log.fail_reason if call_log.fail_reason
				queue =  QueuedCall.find_by_call_log_id(call_log.id) 
				queues << queue.id if queue
			end
			self.queue_call_id = queues
			self.save
			queues
		end

		def repeat?
			self.schedule_type == ReminderSchedule::TYPE_DAILY
		end

		def in_schedule_date? date
			schedule.occurs_on? date if schedule
		end

		def create_start_date!
			self.start_date = Ext::Parser::DateParser.parse(self.client_start_date)
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
					phone_numbers.push address if contact and contact.evaluate? conditions
				end
			else
				phone_numbers = addresses
			end
			phone_numbers
		end

		def create_schedule_recurrence!
			if schedule_type == ReminderSchedule::TYPE_DAILY
				rule = IceCube::Rule.weekly(recursion)
				days.split(",").each do |wday|
					rule.day Ext::Weekday.new(wday).symbol
				end
			end

			self.schedule = IceCube::Schedule.new(start = from_date_time, :duration => to_date_time.to_i - from_date_time.to_i)
			self.schedule.add_recurrence_rule rule if rule
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
			date_time_from_string("#{start_date.to_s} #{time_from}")
		end

		def to_date_time
			date_time_from_string("#{start_date.to_s} #{time_to}")
		end

		def date_time_from_string date_time_string, format = ReminderSchedule::DEFAULT_DATE_TIME_FORMAT
			Ext::Parser::TimeParser.parse(date_time_string, format, project.time_zone)
		end

	end

end