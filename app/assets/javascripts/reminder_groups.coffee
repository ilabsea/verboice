#= require ext/reminder_groups/on_reminder_groups
#= require_tree ./ext/reminder_groups/.
#= require custom_bindings/autocomplete

# We do the check again so tests don't trigger this initialization
onReminderGroups -> if $('#reminder_groups-main').length > 0
  match = window.location.toString().match(/\/projects\/(\d+)\/reminder_groups/)
  project_id = parseInt(match[1])

  window.model = new ReminderGroupMainViewModel(project_id)
  ko.applyBindings(window.model)

  $.get "/ext/projects/#{project_id}/reminder_groups.json", (data) ->
    window.model.reminder_groups $.map(data, (x) -> new ReminderGroup(x))
    window.model.is_ready(true)