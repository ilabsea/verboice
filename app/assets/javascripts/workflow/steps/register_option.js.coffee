#= require workflow/steps/input_setting

onWorkflow ->
  class window.RegisterOption extends window.InputSetting
    constructor: (attrs) ->
      super(attrs)
      @current_caller = ko.observable attrs.current_caller

      @content_kind = ko.observable (if attrs.variable? and attrs.variable != ''
          'variable'
        else if attrs.step? and attrs.step != ''
          'step'
        else if attrs.response? and attrs.response != ''
          'response'
        else
          'current_caller')

      @content_kind_changed = ko.computed =>
        @current_caller("current_caller") if @content_kind() == "current_caller"

    content_kinds: () =>
      return [{text: 'Caller number', value: 'current_caller'},
      {text: 'Variable', value: 'variable'},
      {text: 'Step', value: 'step'},
      {text: 'Response', value: 'response'}]

    to_hash: () =>
      $.extend(super,
        current_caller: if @content_kind() == 'current_caller' then @current_caller() else null
      )