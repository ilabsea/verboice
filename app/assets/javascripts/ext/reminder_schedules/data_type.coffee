onReminderSchedules ->
  class @DataType
    @CONST_NUMBER = 'number'
    @NUMBER = new DataType({id: 1, name: '(number)', code: 'number'})
    @DAY = new DataType({id: 2, name: 'day(s)', code: 'day'})
    @WEEK = new DataType({id: 3, name: 'week(s)', code: 'week'})
    @MONTH = new DataType({id: 4, name: 'month(s)', code: 'month'})
    @YEAR = new DataType({id: 5, name: 'year(s)', code: 'year'})

    constructor: (data) ->
      @id = ko.observable data?.id
      @name = ko.observable data?.name
      @code = ko.observable data?.code

    toJSON: ->
      id: @id()
      name: @name()
      code: @code()
