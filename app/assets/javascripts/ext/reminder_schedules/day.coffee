onReminderSchedules ->
  class @Day
    @SUNDAY: => new Day({id: 0, name: 'Sun', selected: false})
    @MONDAY: => new Day({id: 1, name: 'Mon', selected: false})
    @TUESDAY: => new Day({id: 2, name: 'Tue', selected: false})
    @WEDNESDAY: => new Day({id: 3, name: 'Wed', selected: false})
    @THURSDAY: => new Day({id: 4, name: 'Thu', selected: false})
    @FRIDAY: => new Day({id: 5, name: 'Fri', selected: false})
    @SATURDAY: => new Day({id: 6, name: 'Sat', selected: false})
    @WEEKDAYS: =>
      [
        Day.SUNDAY()
        Day.MONDAY()
        Day.TUESDAY()
        Day.WEDNESDAY()
        Day.THURSDAY()
        Day.FRIDAY()
        Day.SATURDAY()
      ]

    constructor: (data) ->
      @id = ko.observable(data?.id)
      @name = ko.observable(data?.name)
      @selected = ko.observable data?.selected

    toJSON: =>
      id: @id()
      name: @name()
      selected: @selected

    @selected_weekdays: (days) =>
      weekdays = @WEEKDAYS()

      if days
        selected = days.split(",")
        for day in weekdays
          for i in selected
            day.selected(true) if day.id() == parseInt(i)

      weekdays
