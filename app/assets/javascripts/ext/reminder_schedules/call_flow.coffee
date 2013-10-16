onReminderSchedules ->
  class @CallFlow
    constructor: (data) ->
      @id = ko.observable(data?.id)
      @name = ko.observable(data?.name)

      @name_error = ko.computed => if @has_name() then null else "the call flow's name is missing"

      @valid = ko.computed =>
        !@name_error()

    has_name: => $.trim(@name()).length > 0

    toJSON: =>
      id: @id()
      name: @name()
