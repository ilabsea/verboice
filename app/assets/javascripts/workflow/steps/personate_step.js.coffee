#= require workflow/steps/step

onWorkflow ->
  class window.Personate extends Step
    @type = 'personate'

    constructor: (attrs) ->
      super(attrs)

      @current_editing_resource = ko.observable null
      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @option = ko.observable if attrs?.option then new PersonateOption(attrs.option) else new PersonateOption({})

    button_class: () =>
      'luser'

    @add_to_steps: () ->
      workflow.add_step(new Personate)

    @initialize: (hash) ->
      impersonate = new Personate(hash)
      return impersonate

    to_hash: () =>
      $.extend(super,
        option: @option().to_hash()
      )
