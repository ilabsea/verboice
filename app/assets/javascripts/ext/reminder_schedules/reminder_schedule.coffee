onReminderSchedules ->
  class @ReminderSchedule
    @NO_RETRIES = false
    @RETRIES = true
    @NO_REPEAT = '0'
    @REPEAT = '1'
    @DEFAULT_RECUR = 1

    constructor: (data) ->
      @id = ko.observable data?.id
      @reminder_group = ko.observable if data?.reminder_group_id then window.model.find_reminder_group data?.reminder_group_id else new ReminderGroup
      @call_flow = ko.observable if data?.call_flow_id then window.model.find_call_flow data?.call_flow_id else new CallFlow

      @current_reminder_channel = false

      @reminder_channels = ko.observableArray()

      if data?.reminder_channels
        for reminder_channel in data?.reminder_channels
          @addReminderChannelFromData(reminder_channel)
      

      @new_channel_name = ko.observable()
      @new_channel_name_duplicated = ko.computed => if @has_new_channel_name() and @channel_exists(@new_channel_name()) then true else false

      @repeat = ko.observable data?.schedule_type ? ReminderSchedule.NO_REPEAT
      @is_repeat = ko.computed =>
        parseInt(@repeat()) is parseInt(ReminderSchedule.REPEAT)

      @repeat_enable_css = ko.observable 'cb-enable'
      @repeat_disable_css = ko.observable 'cb-disable'
      @repeat_init = ko.computed =>
        if @is_repeat()
          @repeat_enable_css 'cb-enable selected'
          @repeat_disable_css 'cb-disable'
        else
          @repeat_enable_css 'cb-enable'
          @repeat_disable_css 'cb-disable selected'

      @weekdays = ko.observableArray Day.selected_weekdays(data?.days)

      @retries_in_hours = ko.observable data?.retries_in_hours
      @retries = ko.observable data?.retries ? ReminderSchedule.NO_RETRIES
      @is_retries = ko.computed =>
        @retries() is ReminderSchedule.RETRIES

      @retries_enable_css = ko.observable 'cb-enable'
      @retries_disable_css = ko.observable 'cb-disable'
      @retries_init = ko.computed =>
        if @is_retries()
          @retries_enable_css 'cb-enable selected'
          @retries_disable_css 'cb-disable'
        else
          @retries_enable_css 'cb-enable'
          @retries_disable_css 'cb-disable selected'

      @start_date = ko.observable data?.start_date_display
      @from_time = ko.observable data?.time_from
      @to_time = ko.observable data?.time_to
      # repeat
      @recur = ko.observable data?.recursion ? ReminderSchedule.DEFAULT_RECUR

      #conditions
      @conditions = ko.observableArray if data?.conditions then $.map(data.conditions, (x) -> new Condition(x)) else []
      @conditions_description = ko.computed =>
        items = []
        condition_items = $.map(@conditions(), (x) -> x.toJSON() if x.valid())
        for condition in condition_items
          item = []
          item.push if items.length > 0 then and_text else when_text
          item.push "Today - " if condition.data_type != "number"
          item.push condition.variable
          item.push condition.operator
          item.push condition.value
          item.push if condition.data_type == "number" then "" else condition.data_type + if parseInt(condition.value) > 1 then "s" else ""
          items.push item.join(" ")
        items.join(" ")

      @current_condition = ko.observable null

      @hasFocus = ko.observable(false)

      @reminder_group_name = ko.computed =>
        if @reminder_group() then @reminder_group().name else ""

      @channel_name = ko.computed =>
        $.map(@reminder_channels(), (x) -> x.channel().name() unless x.channel() is undefined).join(", ")

      @call_flow_name = ko.computed =>
        if @call_flow() then @call_flow().name else ""

      @reminder_group_error = ko.computed => if @has_reminder_group() then null else "the reminder schedule's call flow is missing"
      @call_flow_error = ko.computed => if @has_call_flow() then null else "the reminder schedule's call flow is missing"
      @channel_error = ko.computed => if @has_channel() then null else "the reminder schedule's channel is missing"
      @start_date_error = ko.computed => if @has_start_date() then null else "the reminder schedule's client start date is missing"
      @from_time_error = ko.computed => if @has_from_time() then null else "the reminder schedule's from time is missing"
      @to_time_error = ko.computed => if @has_to_time() then null else "the reminder schedule's to time is missing"
      @call_time_error = ko.computed => if @is_time_range_valid(@from_time(), @to_time()) then null else "the reminder schedule's call time is missing"
      @days_error = ko.computed => 
        if @is_repeat() && !@has_days_selected() then true else false
      @recur_error = ko.computed => if @has_recur() then null else "the reminder schedule's recur is missing"
      @retries_error = ko.computed => if !@is_retries() or (@is_retries() and @is_retries_in_hours_valid()) then null else "the reminder schedule's retries is missing"

      @error = ko.computed =>
        @reminder_group_error() || @call_flow_error() || @channel_error() || @days_error() || @start_date_error() || @from_time_error() || @to_time_error() || @call_time_error() || @recur_error() || @retries_error()
      @valid = ko.computed => !@error()

    removeReminderChannelByChannel: (reminder_channel) =>

      if reminder_channel?.id()
        @current_reminder_channel = reminder_channel
        json = {_method: 'POST', reminder_channel_id: reminder_channel.id }
        url = "/ext/projects/#{window.model.project_id()}/reminder_schedules/" + @id() + "/remove_reminder_channel.json"
        $.post url, json, @removeCallback
      else  
        @reminder_channels.remove(reminder_channel)

    removeCallback: (response) =>
      @reminder_channels.remove(@current_reminder_channel)

    addReminderChannelFromData: (reminder_channel_data) =>
      channel = @findChannelById(reminder_channel_data.channel_id)
      reminder_channel = new ReminderChannel(channel: channel, reminder_schedule_id: @id(), id: reminder_channel_data.id )
      @reminder_channels.push(reminder_channel)
    
    addReminderChannelByChannel: =>
      existed_channel = @findReminderChannelFromChannelName(@new_channel_name() )
      if existed_channel
         # show error
      else
        channel = @findChannelByName(@new_channel_name())
        if channel
          reminder_channel = new ReminderChannel(channel: channel, reminder_schedule_id: @id() ) 
          @reminder_channels.push(reminder_channel)
          @new_channel_name("")

    findReminderChannelFromChannelName: (channel_name) =>
      return reminder_channel for reminder_channel in @reminder_channels() when reminder_channel.channel().name() == channel_name       

    findChannelById: (id) =>
      return channel for channel in window.model.channels() when channel.id() == id

    findChannelByName: (name) =>
      return channel for channel in window.model.channels() when channel.name() == name      

    repeat_enable: =>
      @repeat(ReminderSchedule.REPEAT)

    repeat_disable: =>
      @repeat(ReminderSchedule.NO_REPEAT)

    day_selected: (day) =>
      day.selected !day.selected()

      # refresh observableArray
      data = @weekdays().slice(0);
      @weekdays([]);
      @weekdays(data);

    retries_enable: =>
      @retries(ReminderSchedule.RETRIES)

    retries_disable: =>
      @retries(ReminderSchedule.NO_RETRIES)

    add_condition: =>
      condition = new Condition()
      @conditions.push(condition)
      @current_condition(condition)

    show_condition: (condition) =>
      @current_condition(condition)

    cancel_condition_edition: =>
      @conditions.remove(@current_condition())
      @current_condition(null)

    close_condition_edition: =>
      @current_condition(null)

    remove_condition: (condition) => 
      @current_condition(null)
      @conditions.remove(condition)

    has_reminder_group: => if @reminder_group() and @reminder_group().valid() then true else false
    has_call_flow: => if @call_flow() and @call_flow().valid() then true else false
    has_channel: => if @reminder_channels().length > 0  then true else false
    has_new_channel_name: => if $.trim(@new_channel_name()).length > 0 then true else false
    has_start_date: => $.trim(@start_date()).length > 0
    has_from_time: => $.trim(@from_time()).length >= 3 and @is_time($.trim(@from_time()))
    has_to_time: => $.trim(@to_time()).length >= 3 and @is_time($.trim(@to_time()))
    is_time: (time_string) =>
      hour_minutes = time_string.split(":")
      if hour_minutes.length is 2 and (parseInt(hour_minutes[0]) >= 0 and parseInt(hour_minutes[1]) >= 0) then true else false
    is_time_range_valid: (from, to) =>
      valid = false
      if @has_from_time() and @has_to_time()
        from_times = from.split(":")
        to_times = to.split(":")
        if parseInt(to_times[0]) > parseInt(from_times[0])
          valid = true
        else if parseInt(to_times[0]) == parseInt(from_times[0]) and parseInt(to_times[1]) > parseInt(from_times[1])
          valid = true
      else
        valid = true
      valid

    is_numeric: (ch) => !isNaN(parseInt(ch))
    has_recur: => 
      is_numeric = true
      for ch in @recur()
        unless @is_numeric(ch)
          is_numeric = false
          break
      $.trim(@recur()).length > 0 && is_numeric && parseInt(@recur()) > 0
    has_days_selected: => if $.map(@weekdays(), (x) -> x if x.selected() == true).length > 0 then true else false
    has_conditions: => if $.map(@conditions(), (x) -> x).length > 0 then true else false
    has_retries: => $.trim(@retries_in_hours()).length > 0
    is_retries_in_hours_valid: => @has_retries() and new RegExp("^[0-9\.]+(,[0-9\.]+)*$").test(@retries_in_hours())

    channel_exists: (channel_name) =>
      $.map(@reminder_channels(), (x) -> x if x.channel().name() == channel_name).length > 0

    toJSON: =>
      reminder_id = @id()

      reminder_group_id: @reminder_group().id()
      call_flow_id: @call_flow().id()
      reminder_channels_attributes: $.map(@reminder_channels(), (reminder_channel) -> reminder_channel.toJSON() unless reminder_channel.channel() is undefined)
      client_start_date: @start_date()
      time_from: @from_time()
      time_to: @to_time()
      schedule_type: @repeat()
      days: $.map(@weekdays(), (x) -> x.id() if x.selected() == true).join(",") if @is_repeat()
      recursion: @recur() if @is_repeat()
      conditions: $.map(@conditions(), (x) -> x.toJSON() if x.valid())
      retries: @retries()
      retries_in_hours: @retries_in_hours() if @is_retries()
      