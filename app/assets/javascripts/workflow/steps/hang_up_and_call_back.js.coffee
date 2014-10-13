#= require workflow/steps/step

onWorkflow ->
  class window.HangUpAndCallBack extends Step
    @type = 'hang_up_and_call_back'

    constructor: (attrs) ->
      super(attrs)
      @dial_prefix = ko.observable attrs.dial_prefix

      @call_flow_options = ko.observableArray call_flows
      @selected_call_flow_id = ko.observable attrs.selected_call_flow_id
      @retries = ko.observable attrs.retries

    button_class: =>
      'lcallback'

    default_name: =>
      'Call back'

    to_hash: () =>
      $.extend(super,
        dial_prefix: @dial_prefix(),
        selected_call_flow_id: @selected_call_flow_id(),
        retries: @retries()
      )

    @add_to_steps: () ->
      workflow.add_step(new HangUpAndCallBack)

    @initialize: (hash) ->
      step = new HangUpAndCallBack(hash)
      return step
