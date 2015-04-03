#= require workflow/steps/step

onWorkflow ->
  class window.Personate extends Step
    @type = 'personate'

    constructor: (attrs) ->
      super(attrs)

      @option = ko.observable if attrs?.option then new PersonateOption(attrs.option) else new PersonateOption({})

    button_class: () =>
      'control_step impersonate'

    @add_to_steps: () ->
      workflow.add_step(new Personate)

    @initialize: (hash) ->
      impersonate = new Personate(hash)
      return impersonate

    to_hash: () =>
      $.extend(super,
        option: @option().to_hash()
      )

    default_name: () =>
      'Transfer Data'
