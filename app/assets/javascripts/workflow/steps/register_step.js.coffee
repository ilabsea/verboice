#= require workflow/steps/step

onWorkflow ->
  class window.Register extends Step
    @type = 'register'

    constructor: (attrs) ->
      super(attrs)

      @option = ko.observable if attrs?.option then new RegisterOption(attrs.option) else new RegisterOption({})
      @store = ko.observable attrs.store
      @defines_store = ko.observable !!attrs.store

      @reminder_groups = ko.observableArray reminder_groups.map (type) -> type.name
      @reminder_group = ko.observable attrs.reminder_group

      @current_editing_resource = ko.observable null
      @resources =
        confirmation: new ResourceEditor(@, attrs.confirmation_resource)

      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_confirmation_resource_invalid = ko.computed () =>
        not @resources.confirmation.is_valid()

      @is_reminder_group_invalid = ko.computed () =>
        $.inArray(@reminder_group(), @reminder_groups()) is -1

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_confirmation_resource_invalid() or @is_reminder_group_invalid()

    button_class: () =>
      'control_step register'

    @add_to_steps: () ->
      workflow.add_step(new Register)

    @initialize: (hash) ->
      step = new Register(hash)
      return step

    to_hash: () =>
      $.extend(super,
        store: (if @store() then @store() else null)
        reminder_group: @reminder_group()
        confirmation_resource: @resources.confirmation.to_hash()
        option: @option().to_hash()
      )

    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_confirmation_resource: () =>
      @show_resource('confirmation')