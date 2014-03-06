#= require ext/reminder_schedules/on_reminder_schedules
#= require_tree ./ext/reminder_schedules/.

# We do the check again so tests don't trigger this initialization
onReminderSchedules -> if $('#reminder_schedules-main').length > 0
  match = window.location.toString().match(/\/projects\/(\d+)\/reminder_schedules/)
  project_id = parseInt(match[1])

  window.model = new MainViewModel(project_id)
  ko.applyBindings(window.model)

  $.get "/ext/projects/#{project_id}/reminder_schedules/references_data.json", (data) ->
    window.model.channels $.map(data.channels, (x) -> new Channel(x))
    window.model.call_flows $.map(data.call_flows, (x) -> new CallFlow(x))
    window.model.reminder_groups $.map(data.reminder_groups, (x) -> new ReminderGroup(x))
    window.model.variables $.map(data.variables, (x) -> new Variable(x))
    $.get "/ext/projects/#{project_id}/reminder_schedules.json", (data) ->
      window.model.reminder_schedules $.map(data, (x) -> new ReminderSchedule(x))
      window.model.is_ready(true)
  