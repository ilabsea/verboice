#= require workflow/steps/input_setting

onWorkflow ->
  class window.ExternalStepSetting extends window.InputSetting
    constructor: (parent, attrs) ->
      super(attrs)

      @name = attrs.name
      @display_name = attrs.display_name
      @type = attrs.type
      @required = attrs.required
      @parent = parent

    to_hash: () =>
      $.extend super(), {
        name: @name
        display_name: @display_name
        required: @required
      }

    is_datatype_date: ->
      if @type == "date_type" then true else false

    visible_textbox_value: ->
      if not @is_datatype_date() and @content_kind() is "value" then true else false

    date_types: ->
      ['Day', 'Week', 'Month']

    is_required: ->
      if @required == 'true' then true else false

    is_invalid: () =>
      @is_required() and !@has_value()
      
    has_value: () =>
      setting = @to_hash()
      !(setting['value'] == null or setting['value'] == undefined) or !(setting['step'] == null or setting['step'] == undefined) or !(setting['variable'] == null or setting['variable'] == undefined) or !(setting['response'] == null or setting['response'] == undefined)

    description: () =>
      desc = super()
      if desc? then "(#{desc})" else null

    on_begin_edition: () =>
      @content_kind_tmp = @content_kind()
      @value_tmp = @value()
      @variable_tmp = @variable()
      @step_id_tmp = @step_id()

    save: () =>
      @exit()

    cancel: () =>
      @content_kind(@content_kind_tmp)
      @value(@value_tmp)
      @variable(@variable_tmp)
      @step_id(@step_id_tmp)
      @exit()

    exit: () =>
      @parent.current_editing_setting(null)
