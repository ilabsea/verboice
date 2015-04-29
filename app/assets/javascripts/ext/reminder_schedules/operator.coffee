onReminderSchedules ->
  class @Operator
    @LESS_THAN = new Operator({id: 1, name: 'is less than', code: '<'})
    @LESS_THAN_OR_EQUAL = new Operator({id: 1, name: 'is less or equal to', code: '<='})
    @EQUAL = new Operator({id: 1, name: 'is equal to', code: '='})
    @GREATER_THAN_OR_EQUAL = new Operator({id: 1, name: 'is greater or equal to', code: '>='})
    @GREATER_THAN = new Operator({id: 1, name: 'is greater than', code: '>'})

    constructor: (data) ->
      @id = ko.observable data?.id
      @name = ko.observable data?.name
      @code = ko.observable data?.code

    toJSON: ->
      id: @id()
      name: @name()
      code: @code()