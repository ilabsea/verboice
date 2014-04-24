onReminderSchedules ->
  class @MainViewModel
    constructor: (project_id) ->
      @project_id = ko.observable project_id
      @reminder_schedules = ko.observableArray()
      @reminder_groups = ko.observableArray()
      @channels = ko.observableArray()
      @call_flows = ko.observableArray()
      @variables = ko.observableArray()
      @operators = ko.observableArray([
        Operator.EQUAL
        Operator.LESS_THAN
        Operator.LESS_THAN_OR_EQUAL
        Operator.GREATER_THAN_OR_EQUAL
        Operator.GREATER_THAN
      ])
      @data_types = ko.observableArray([
        DataType.DAY
        DataType.WEEK
        DataType.MONTH
        DataType.YEAR
      ])

      @is_ready = ko.observable false

      @currentReminderSchedule = ko.observable()

      @optionsScheduleTypes = ko.observableArray $.map([{name: "No", id: 0}, {name: "Yes", id: 1}], (x) -> [[x.name, x.id]])

      @savingReminderSchedule = ko.observable(false)
      @channel_name = ko.observable()
      
    newReminderSchedule: =>
      reminderSchedule = new ReminderSchedule({id: 0})
      @reminder_schedules.push(reminderSchedule)
      @currentReminderSchedule(reminderSchedule)
      reminderSchedule.hasFocus(true)

    editReminderSchedule: (reminderSchedule) =>
      if @currentReminderSchedule() then @reminder_schedules.remove(@currentReminderSchedule()) unless @currentReminderSchedule().id()
      @currentReminderSchedule(reminderSchedule)
      reminderSchedule.hasFocus(true)

    cancelReminderSchedule: =>
      @reminder_schedules.remove(@currentReminderSchedule()) unless @currentReminderSchedule().id()
      @currentReminderSchedule(null)

    saveReminderSchedule: =>
      @savingReminderSchedule(true)
      json = {ext_reminder_schedule: @currentReminderSchedule().toJSON()}
      console.log('json' ,json)
      if @currentReminderSchedule().id()
        json._method = 'put'
        $.post "/ext/projects/#{@project_id()}/reminder_schedules/#{@currentReminderSchedule().id()}.json", json, @saveReminderScheduleCallback
      else
        $.post "/ext/projects/#{@project_id()}/reminder_schedules.json", json, @saveReminderScheduleCallback

    saveReminderScheduleCallback: (data) =>
      #if reminder schedule is new, we need to set id
      $.status.showNotice("Reminder schedule successfully #{if @currentReminderSchedule().id() then 'saved' else 'created'}", 2000)
      @currentReminderSchedule().id(data.id)

      @currentReminderSchedule(null)
      @savingReminderSchedule(false)
      window.location.href = "/ext/projects/#{@project_id()}/reminder_schedules";

    deleteReminderSchedule: (reminderSchedule) =>
      if confirm("Are you sure you want to delete this reminder schedule?")
        $.post "/ext/projects/#{@project_id()}/reminder_schedules/#{reminderSchedule.id()}.json", {_method: 'delete'}, =>
          @reminder_schedules.remove(reminderSchedule)
          $.status.showNotice("Reminder schedule successfully deleted", 2000)

    find_channel: (id) =>
      return channel for channel in @channels() when channel.id() == id

      

    find_call_flow: (id) =>
      return call_flow for call_flow in @call_flows() when call_flow.id() == id

    find_reminder_group: (id) =>
      return reminder_group for reminder_group in @reminder_groups() when reminder_group.id() == id

    find_variable: (name) =>
      return variable for variable in @variables() when variable.name() == name

    find_operator: (code) =>
      return operator for operator in @operators() when operator.code() == code

    find_data_type: (code) =>
      return data_type for data_type in @data_types() when data_type.code() == code
