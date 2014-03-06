require 'spec_helper'

describe Ext::ReminderSchedule  do
  	

  before(:each) do
	@project = Project.make(:time_zone => 'Bangkok')
    @call_flow= CallFlow.make :project_id => @project.id
    @channel = Channels::Custom.make :call_flow => @call_flow, :config => {prefix: '070, 010'}

    @reminder_group = Ext::ReminderGroup.make
  end

  it "should project time zone is 'Bangkok' +0070" do
    @project.time_zone.should eq 'Bangkok'
	(ActiveSupport::TimeZone.new(@project.time_zone).utc_offset / (60 * 60)).should eq 7
  end

  describe 'migration from single to multiple channel' do
  	it "should migrate data from single to multiple channel" do
  		@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,

	  		:reminder_group_id => @reminder_group.id,
	  		:schedule => nil,
	  		:client_start_date => "05/10/2012",
	  		:time_from => "08:00",
	  		:time_to => "17:00",
	  	}
	  	channel1 = Channels::Custom.make :call_flow => @call_flow, :config => {prefix: '070, 010'}
	  	channel2 = Channels::Custom.make :call_flow => @call_flow, :config => {prefix: '070, 010'}
	  	channel3 = Channels::Custom.make :call_flow => @call_flow, :config => {prefix: '070, 010'}


	  	# there 3 reminders with appropiate channel
	  	reminder1 = Ext::ReminderSchedule.make @attr.merge(channel_id: channel1.id)
	  	reminder2 = Ext::ReminderSchedule.make @attr.merge(channel_id: channel2.id)
	  	reminder3 = Ext::ReminderSchedule.make @attr.merge(channel_id: channel3.id)

	  	reminder4 = Ext::ReminderSchedule.make @attr
	  	reminder5 = Ext::ReminderSchedule.make @attr
	  	reminder6 = Ext::ReminderSchedule.make @attr



	  	# reminder channel should be empty for each reminder
	  	reminder1.reminder_channels.count.should eq 0
	  	reminder2.reminder_channels.count.should eq 0
	  	reminder3.reminder_channels.count.should eq 0

	  	# migrating channel to reminder_channel
	  	Ext::ReminderSchedule.channel_migrate_reminder_schedule

	  	# expecting the following result
	  	reminder1.reminder_channels(true).count.should eq 1
	  	reminder1.reminder_channels(true).first.channel.should eq channel1

	  	reminder2.reminder_channels(true).count.should eq 1
	  	reminder2.reminder_channels(true).first.channel.should eq channel2

	  	reminder3.reminder_channels(true).count.should eq 1
	  	reminder3.reminder_channels(true).first.channel.should eq channel3
  	end
  end

  describe 'suggested_channel_for' do
    before(:each) do
      @channel0 = Channels::Custom.make :call_flow => @call_flow	
      @channel1 = Channels::Custom.make :call_flow => @call_flow, :config => { 'prefix' => '010,012' }
      @channel2 = Channels::Custom.make :call_flow => @call_flow, :config => { 'prefix' => '015,017' }
      @channel3 = Channels::Custom.make :call_flow => @call_flow, :config => { 'prefix' => '080,070' }
     
      conditions = [Ext::Condition.new("var1", "=", "5", "number")]
      @attr = { :schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
		  	    :project_id => @project.id,
		  		:call_flow_id => @call_flow.id,
		  		:reminder_group_id => @reminder_group.id,
		  		:client_start_date => "25/10/2012",
		  		:time_from => "10:00",
		  		:time_to => "12:00",
		  		:recursion => 1,
		  		:retries => true,
		  		:retries_in_hours => "1,1" 
	  }

	  @reminder = Ext::ReminderSchedule.create(@attr.merge(:client_start_date => "05/10/2012", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))

	  Ext::ReminderChannel.create! channel_id: @channel0.id, reminder_schedule_id: @reminder.id
      Ext::ReminderChannel.create! channel_id: @channel1.id, reminder_schedule_id: @reminder.id
      Ext::ReminderChannel.create! channel_id: @channel2.id, reminder_schedule_id: @reminder.id
      Ext::ReminderChannel.create! channel_id: @channel3.id, reminder_schedule_id: @reminder.id
    end

    it 'should return the channel with prefix that match the given address ' do

      [
      	 {address: '0700001222', channel: @channel3},
      	 {address: '0800001222', channel: @channel3},
      	 {address: '010200202', channel: @channel1},
      	 {address: '012001222', channel: @channel1},
      	 {address: '015', channel: @channel2},
      
      ].each do |item|
       	   @reminder.suggested_channel_for(item[:address]).should eq item[:channel]
  		end
    end

    it 'should return the first channel if no preifx is matched again the given address' do
    	@reminder.suggested_channel_for('09900010').should eq @channel0
    end
  end

  describe 'address_matched_suggest?' do
  	it 'should return true if address matched suggestion' do
  	   addresses = [ '012101010', "070123456", '85512000111']
  	   suggestion = "070,012,855"

  	   addresses.each do |address|
  	     Ext::ReminderSchedule.address_matched_suggest?(address, suggestion).should eq true
  	   end
  	end

  	it 'should return false if address does not matched the suggestion' do
  	   addresses = [ '012101010', "070123456", '85512000111']
  	   suggestion = "010,0123,8552"

  	   addresses.each do |address|
  	     Ext::ReminderSchedule.address_matched_suggest?(address, suggestion).should eq false
  	   end
  	end
  end

  describe "Create with nested attributes" do
	before(:each) do	
	  @attr = { :schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
		  	    :project_id => @project.id,
		  		:call_flow_id => @call_flow.id,
		  		:reminder_group_id => @reminder_group.id,
		  		:client_start_date => "25/10/2012",
		  		:time_from => "10:00",
		  		:time_to => "12:00",
		  		:recursion => 1,
		  		:retries => true,
		  		:retries_in_hours => "1,1",

		    	:reminder_channels_attributes => [ {channel_id: 1, reminder_schedule_id: 0, id: false }, {channel_id: 2, reminder_schedule_id: 0 } ]
	    	}
    end

    it 'should create reminder_schedule with corresponding reminder_channel' do
      reminder = Ext::ReminderSchedule.new @attr
      reminder.save

      reminder.reminder_channels.count.should eq 2

      reminder.reminder_channels[0].channel_id.should eq 1
      reminder.reminder_channels[0].reminder_schedule_id.should eq reminder.id

      reminder.reminder_channels[1].channel_id.should eq 2
      reminder.reminder_channels[1].reminder_schedule_id.should eq reminder.id
    end

    it 'should update reminder schedule with corresponding reminder_channel' do
       params = { :schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
		  	    :project_id => @project.id,
  		  		:call_flow_id => @call_flow.id,
  		  		:reminder_group_id => @reminder_group.id,
  		  		:client_start_date => "25/10/2012",
  		  		:time_from => "10:00",
  		  		:time_to => "12:00",
  		  		:recursion => 1,
  		  		:retries => true,
  		  		:retries_in_hours => "1,1"
  	   }

       reminder = Ext::ReminderSchedule.create params
       reminder.save.should be_true
       reminder.reminder_channels.count.should eq 0

       reminder.update_attributes(@attr)
       reminder.reminder_channels.count.should eq 2

       reminder.reminder_channels[0].channel_id.should eq 1
       reminder.reminder_channels[0].reminder_schedule_id.should eq reminder.id

       reminder.reminder_channels[1].channel_id.should eq 2
       reminder.reminder_channels[1].reminder_schedule_id.should eq reminder.id
    end
  end
	
	describe "Create new reminder schedule" do
	  before(:each) do
	  	@valid = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:reminder_group_id => @reminder_group.id,
	  		:client_start_date => "25/10/2012",

	  		:schedule => nil,
	  		:time_from => "10:00",
	  		:time_to => "12:00",
	  		:recursion => 1,
	  		:retries => true,
	  		:retries_in_hours => "1,1",
	  		:reminder_channels_attributes => [ {channel_id: 1, reminder_schedule_id: 0, id: false }, {channel_id: 2, reminder_schedule_id: 0 } ]
	  	}
	  end	

	  it "should create a reminder schedule with valid attribute" do 
			reminder_schedule = Ext::ReminderSchedule.new @valid
			reminder_schedule.save.should eq true
			reminder_schedule.schedule_type.should eq Ext::ReminderSchedule::TYPE_ONE_TIME
	  end

	  it "should initialize schedule and schedule retries then bind it" do
	  	reminder_schedule = Ext::ReminderSchedule.new @valid
			reminder_schedule.save.should eq true
			reminder_schedule.schedule.should_not be_nil
			reminder_schedule.retries_schedule.should_not be_nil
			reminder_schedule.retries_schedule.weekdays.should eq("0,1,2,3,4,5,6") # everydays
			reminder_schedule.retries_schedule.disabled.should eq(true)
	  end

	  it "should reset retries_in_hours and retries_schedule to nil when retries is disalbed" do
	  	@valid.merge!(:retries => false)
	  	reminder_schedule = Ext::ReminderSchedule.new @valid
	  	reminder_schedule.save.should eq true
			reminder_schedule.retries_in_hours.should eq nil
			reminder_schedule.retries_schedule.should eq nil
	  end

	  it "should require start_date with valid format" do
	     invalid = @valid.merge(:client_start_date => nil)	
	     reminder_schedule  =  Ext::ReminderSchedule.new invalid
	     reminder_schedule.save().should eq false
	  end

	  it "should require days if type is repeat" do
	  	invalid = @valid.merge(:days => "", :schedule_type => Ext::ReminderSchedule::TYPE_DAILY)	
	    reminder_schedule  =  Ext::ReminderSchedule.new invalid
	    reminder_schedule.save().should eq false
	  end

	  it "should ignore save when it is retries and retries_in_hours is invalid" do
	  	invalid = @valid.merge(:retries => true, :retries_in_hours => "aa")
	    reminder_schedule  =  Ext::ReminderSchedule.new invalid
	    reminder_schedule.save().should eq false
	  end

	  it "should invoke create_queued_calls after creating new reminder_schedule" do
	  	reminder_schedule = Ext::ReminderSchedule.new @valid
	  	reminder_schedule.should_receive(:create_queued_calls)
		  reminder_schedule.save
	  end
	end

	describe "#create_queued_calls" do
		before(:each) do
			@now = DateTime.new(2012,10,25, 9,0,0, "+7") # use the same timezone as reminder schedule
			DateTime.stub!(:now).and_return(@now)
			@addresses = ["1000", "1001"]
			reminder_group = Ext::ReminderGroup.make addresses: @addresses

			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:reminder_group_id => reminder_group.id,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,

	  		:schedule => nil,
	  		:client_start_date => "25/10/2012", #Thursday
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}
		end

		it "should process to enqueue call all contact in addresses" do
			reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "24/10/2012", :time_from => "08:00", :time_to => "17:00"))
	  	reminder.should_receive(:process).with(@addresses, @now)
	  	reminder.create_queued_calls
		end
	end


	describe "#process" do
		before(:each) do
			@now = DateTime.new(2012,10,25, 9,0,0, "+7") # use the same timezone as reminder schedule

			@attr = {
		  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
		  		:reminder_group_id => @reminder_group.id,
		  		:project_id => @project.id,
		  		:call_flow_id => @call_flow.id,

		  		:schedule => nil,
		  		:client_start_date => "25/10/2012", #Thursday
		  		:time_from => "08:00",
		  		:time_to => "17:00",
		  		:reminder_channels_attributes => [ {channel_id: @channel.id, reminder_schedule_id: 0}]
		  	}

	  		@addresses = ["1000", "1001", "1002"]
		end


		it "should current date and time is 2012-10-25 09:00" do
			@now.year.should eq 2012
			@now.month.should eq 10
			@now.day.should eq 25
			@now.hour.should eq 9
			@now.minute.should eq 0
			@now.wday.should eq 4
		end

		describe "start_date in the past" do
			it "should not enqueue call to any contactas" do
			  reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "24/10/2012", :time_from => "08:00", :time_to => "17:00"))
		  	  reminder.should_receive(:enqueued_call).never
		  	  reminder.process @addresses, @now
			end
		end

		describe "start_date is the same as current date" do
			describe "from time is in the past of now" do
				describe "to time is in the past of now" do
					it "should not enqueue call to any contacts" do
						reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "25/10/2012", :time_from => "08:00", :time_to => "08:30"))

						reminder.should_receive(:enqueued_call).with(@addresses, @now).never
						reminder.process @addresses, @now
					end

					it "should enqueue call to any contacts when it's scheduling" do
						reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "25/10/2012", :time_from => "08:00", :time_to => "08:30"))
						reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000", "1001", "1002"])
						reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now)
						reminder.process @addresses, @now, true
					end
				end

				describe "to time is in the future of now" do
					it "should enqueue call to any contacts" do
						reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "25/10/2012", :time_from => "08:00", :time_to => "17:00"))

						reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000", "1001", "1002"])
						reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now)
						reminder.process @addresses, @now
					end
				end
			end

			describe "from time is in the future of now" do
				describe "without repeat" do
					describe "without conditions" do
						it "should enqueue call to any contacts" do
							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "25/10/2012", :time_from => "10:00", :time_to => "17:00"))

							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000", "1001", "1002"])

							reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now)
							reminder.process @addresses, @now
						end
					end

					describe "with conditions" do
						it "should not enqueue call to any contacts when there're no any contacts are matches all" do
							conditions = [Ext::Condition.new("var1", "=", "5", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "25/10/2012", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))

							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return([])
							reminder.should_receive(:enqueued_call).with(@addresses, @now).never
							reminder.process @addresses, @now
						end

						it "should enqueue call to only contacts that matches all" do
							conditions = [Ext::Condition.new("var1", "=", "10", "number"), Ext::Condition.new("var2", "=", "20", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "25/10/2012", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))

							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000"])
							reminder.should_receive(:enqueued_call).with(["1000"], @now)
							reminder.process @addresses, @now
						end
					end
				end

				describe "with repeat" do
					describe "without conditions" do
						it "should enqueue call to any contacts when wday of start_date and now are the same" do
							params = { :schedule_type => Ext::ReminderSchedule::TYPE_DAILY, 
								       :days => "4", :recursion => 1, 
								       :client_start_date => "25/10/2012", 
								       :time_from => "10:00", 
								       :time_to => "17:00"
								       
					    }
							reminder = Ext::ReminderSchedule.make(@attr.merge(params))

							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000", "1001", "1002"])
					        reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now)
					        reminder.process @addresses, @now
						end

						it "should not enqueue call to any contacts when wday of start_date and now are different" do
							@now = DateTime.new(2012,10,26, 9,0,0, "+7") # is Friday

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "25/10/2012", :time_from => "10:00", :time_to => "17:00"))

							reminder.should_receive(:callers_matches_conditions).with(@addresses).never
					  		reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now).never
					  		reminder.process @addresses, @now
						end
					end

					describe "with conditions" do
						it "should not enqueue call any contacts when it's not match at least one but start_date and now are the same" do
							conditions = [Ext::Condition.new("var1", "=", "5", "number"), Ext::Condition.new("var2", "=", "10", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "4", :recursion => 1, :client_start_date => "25/10/2012", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))

							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return([])
					  		reminder.should_receive(:enqueued_call).with([], @now).never
					  		reminder.process @addresses, @now
						end

						it "should not enqueue call any contacts when it's match all but wday of start_date and now are different" do
							@now = DateTime.new(2012,10,26, 9,0,0, "+7") # is Friday
							conditions = [Ext::Condition.new("var1", "=", "5", "number"), Ext::Condition.new("var2", "=", "10", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "25/10/2012", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))


							reminder.should_receive(:callers_matches_conditions).with(@addresses).never
					  		reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now).never
					  		reminder.process @addresses, @now
						end

						it "should enqueue call any contacts when it's match all and wday of start_date and now are the same" do
							conditions = [Ext::Condition.new("var1", "=", "5", "number"), Ext::Condition.new("var2", "=", "10", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "25/10/2012", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))

							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000"])
					  		reminder.should_receive(:enqueued_call).with(["1000"], @now)
					  		reminder.process @addresses, @now
						end
					end
				end
			end
		end

		describe "start_date in the future" do
			describe "is no repeat" do
				it "should not enqueued call to any contacts" do
					reminder = Ext::ReminderSchedule.make @attr.merge(:client_start_date => "26/10/2012", :time_from => "08:00", :time_to => "17:00")
					reminder.should_receive(:callers_matches_conditions).with(@addresses).never
			  	reminder.should_receive(:enqueued_call).with([], @now).never
			  	reminder.process @addresses, @now
				end
			end

			describe "is repeat" do
				it "should not enqueued call to any contacts when wday in schedule days has include wday of today" do
					reminder = Ext::ReminderSchedule.make @attr.merge(:client_start_date => "26/10/2012", :time_from => "08:00", :time_to => "17:00", :schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "4,5", :recursion => 1)
					reminder.should_receive(:callers_matches_conditions).with(@addresses).never
			  	reminder.should_receive(:enqueued_call).with([], @now).never
			  	reminder.process @addresses, @now
				end
			end
		end
	end

	describe ".schedule" do
	  before(:each) do
		@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,

	  		:reminder_group_id => @reminder_group.id,
	  		:schedule => nil,
	  		:client_start_date => "25/10/2012",
	  		:time_from => "08:00",
	  		:time_to => "17:00",
	  		:reminder_channels_attributes => [ {channel_id: @channel.id, reminder_schedule_id: 0, id: false }, {channel_id: @channel.id, reminder_schedule_id: 0 } ]
	  	}

	  	@reminder_schedules = []
	  	@addresses = []
	  	3.times.each do |i|
          @reminder_schedules << Ext::ReminderSchedule.make(@attr.merge(:name => "#{@attr[:name]}-#{i}"))
          @addresses << (1000 + i).to_s
        end

        @at_time = DateTime.new(2012,10,25, 8,1)
	  end

	  it "should process all reminders with addresses" do
		# @reminder_schedules.should_receive(:process)
       Ext::ReminderSchedule.schedule(@project.id, @at_time)
      end
	end

	describe "#enqueued_call" do
		before(:each) do
		  params ={ :schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
			  		:project_id => @project.id,
			  		:call_flow_id => @call_flow.id,
			  		:reminder_group_id => @reminder_group.id,
			  		:schedule => nil,
			  		:client_start_date => "25/10/2012",
			  		:time_from => "08:00",
			  		:time_to => "17:00",
			  		:reminder_channels_attributes => [ {channel_id: @channel.id, reminder_schedule_id: 0, id: false }, {channel_id: @channel.id, reminder_schedule_id: 0 } ]

		  }	
		  @reminder = Ext::ReminderSchedule.make params
		  @addresses = ["1001", "1002", "1003", "1004", "1005", "1006"]
		end

		it "should enqueued call to verboice queued call" do 	
	   	  queues = @reminder.enqueued_call @addresses, DateTime.new(2012,11,26)
	   	  queues.size.should eq 6
		end
	end

	describe "#call_options" do
		before(:each) do
			@schedule = Schedule.make :disabled => true
			@reminder = Ext::ReminderSchedule.make(
		  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
		  		:project_id => @project.id,
		  		:call_flow_id => @call_flow.id,
		  		:reminder_group_id => @reminder_group.id,
		  		:schedule => nil,
		  		:client_start_date => "25/10/2012",
		  		:time_from => "08:00",
		  		:time_to => "17:00",
		  		:retries => true,
		  		:retries_in_hours => "1,1",
		  		:retries_schedule => @schedule,
		  		:reminder_channels_attributes => [ {channel_id: @channel.id, reminder_schedule_id: 0, id: false }, {channel_id: @channel.id, reminder_schedule_id: 0 } ]
		  	)
		end

		it "should create options for schedule enqueued call " do
			options = @reminder.call_options DateTime.new(2012,10,22)
			options[:call_flow_id].should eq @reminder.call_flow_id
			options[:project_id].should eq @reminder.project_id
			# options[:time_zone].should  eq @reminder.project.time_zone
			options[:not_before].should eq DateTime.new(2012, 10, 22, 1, 0)
			options[:schedule_id].should eq @schedule.id
		end
	end

	describe "#has_conditions?" do
		before(:each) do
			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,

	  		:reminder_group_id => @reminder_group.id,
	  		:schedule => nil,
	  		:client_start_date => "25/10/2012",
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}
	  end

		it "should return false when reminder schedule's conditions are not setting up" do
			reminder_schedule = Ext::ReminderSchedule.make @attr

			reminder_schedule.has_conditions?.should be false
		end

		it "should return true when reminder schedule's conditions are setting up" do
			conditions = [Ext::Condition.new("var1", "=", "5", "number")]
			reminder_schedule = Ext::ReminderSchedule.make @attr.merge(:conditions => conditions)

			reminder_schedule.has_conditions?.should be true
		end
	end

	describe "#callers_matches_conditions" do
		before(:each) do
			@attr = {
		  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
		  		:project_id => @project.id,
		  		:call_flow_id => @call_flow.id,

		  		:reminder_group_id => @reminder_group.id,
		  		:schedule => nil,
		  		:client_start_date => "25/10/2012",
		  		:time_from => "08:00",
		  		:time_to => "17:00"
		  	}

			@contact_one = @project.contacts.build
			@contact_one.addresses.build(address: "1000")
			@contact_one.save

			@contact_two = @project.contacts.build
			@contact_two.addresses.build(address: "1001")
			@contact_two.save

			@project_var1 = ProjectVariable.make :name => "var1", project: @project
			@project_var2 = ProjectVariable.make :name => "var2"
			PersistedVariable.make contact_id: @contact_one.id, project_variable_id: @project_var1.id, value: "5"
			PersistedVariable.make contact_id: @contact_one.id, project_variable_id: @project_var2.id, value: "10"
			PersistedVariable.make contact_id: @contact_two.id, project_variable_id: @project_var1.id, value: "15"
			PersistedVariable.make contact_id: @contact_two.id, project_variable_id: @project_var2.id, value: "20"

			@addresses = ["1000", "1001"]
		end

		it "should return an empty array when any contacts are not matches conditions" do
			conditions = [Ext::Condition.new("var1", "=", "99", "number")]
			reminder_schedule = Ext::ReminderSchedule.make @attr.merge(:conditions => conditions)

			phone_numbers = reminder_schedule.callers_matches_conditions @addresses
			phone_numbers.empty?.should be true
		end

		it "should return array of first contact phone number when only the first contact is matches conditions" do
			conditions = [Ext::Condition.new("var1", "=", "5", "number")]
			reminder_schedule = Ext::ReminderSchedule.make @attr.merge(:conditions => conditions)

			phone_numbers = reminder_schedule.callers_matches_conditions @addresses
			phone_numbers.size.should be 1
		end

		it "should return array of all contacts phone number when conditions are not setting up" do
			reminder_schedule = Ext::ReminderSchedule.make @attr

			phone_numbers = reminder_schedule.callers_matches_conditions @addresses
			phone_numbers.size.should be @addresses.size
		end
	end

end