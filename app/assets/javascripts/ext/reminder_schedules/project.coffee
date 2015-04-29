onReminderSchedules ->
  class @Project
    constructor: (data) ->
      @id = ko.observable(data?.id)
      @name = ko.observable(data?.name)

    toJSON: =>
      id: @id()
      name: @name()
