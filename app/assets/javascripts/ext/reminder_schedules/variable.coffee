onReminderSchedules ->
  class @Variable
    constructor: (data) ->
      @id = ko.observable(data?.id)
      @name = ko.observable(data?.name)
      @type = ko.observable data?.type

    toJSON: =>
      id: @id()
      name: @name()
      type: @type()