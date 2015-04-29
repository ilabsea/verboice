#= require workflow/steps/step

onWorkflow ->
  class window.Deregister extends Step
    @type = 'deregister'

    constructor: (attrs) ->
      super(attrs)

      @reminder_groups = ko.observableArray (reminder_groups.map (type) -> type.name).concat ["All"]
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
      'control_step deregister'

    @add_to_steps: () ->
      workflow.add_step(new Deregister)

    @initialize: (hash) ->
      step = new Deregister(hash)
      return step

    to_hash: () =>
      $.extend(super,        
        reminder_group: @reminder_group()
        confirmation_resource: @resources.confirmation.to_hash()
      )

    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_confirmation_resource: () =>
      @show_resource('confirmation')