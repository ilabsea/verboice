onReminderSchedules ->
  class @Condition
    constructor: (data) ->
      @variable = ko.observable if data?.variable then window.model.find_variable data?.variable else new Variable
      @operator = ko.observable if data?.operator then window.model.find_operator data?.operator else new Operator
      @value = ko.observable data?.value
      @data_type = ko.observable if data?.data_type then window.model.find_data_type data?.data_type else new DataType

      @variable_name = ko.computed => if @variable() then @variable().name else "undefined"
      @operator_name = ko.computed => if @operator() then @operator().name else "undefined"
      @operator_code = ko.computed => if @operator() then @operator().code else "undefined"
      @value_text = ko.computed => if @value() then @value() else "undefined"
      @data_type_name = ko.computed => 
        data_type_name = ""
        if @data_type()
          if @data_type().code() != DataType.CONST_NUMBER
            data_type_name = @data_type().code() + if parseInt(@value()) > 1 then "s" else ""
        else
          data_type_name = "undefined"

        data_type_name

      @is_date_time = ko.computed =>
        if @data_type()?.code() == DataType.CONST_NUMBER then false else true

      @variable_error = ko.computed => if @variable() then false else true
      @operator_error = ko.computed => if @operator() then false else true
      @value_error = ko.computed => if @value() then false else true
      @data_type_error = ko.computed => if @data_type() then false else true

      @error = ko.computed => 
        @variable_error() || @operator_error() || @value_error() || @data_type_error()

    toJSON: =>
      variable: @variable().name()
      operator: @operator().code()
      value: @value()
      data_type: @data_type().code()

    valid: =>
      !@error()