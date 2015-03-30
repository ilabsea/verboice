onWorkflow ->
  class window.CommandSelector
    constructor: (requestor) ->
      @handlers = (new ClassBindingHandler(klass, @) for klass in @enabled_command_types())
      @commands = ko.observableArray(@handlers)
      @requestor = requestor

      @standard_commands = ko.computed =>
        (command for command in @commands() when !command.cmd.is_external?())

      @external_commands = ko.computed =>
        (command for command in @commands() when command.cmd.is_external?())

    display_template_id: () ->
      'command_selector_template'

    with_requestor: (requestor) =>
      @requestor = requestor
      if requestor.excluded_types?
        @commands(handler for handler in @handlers when handler.cmd not in requestor.excluded_types)
      else
        @commands(@handlers)
      return @

    enabled_command_types: () ->
      for step_type in step_types
        if step_type is Nuntium
          if nuntium_configured then step_type else continue
        else if step_type is SpeechRecognition
          if step_asr_enabled then step_type else continue
        else if step_type is Impersonate
          if step_impersonate_enabled then step_type else continue
        else
          step_type

  class window.AddRootRequestor
    command_selected: (cmd_type) =>
      new_step = workflow.create_step(cmd_type, null)
      workflow.set_as_current(new_step)

  class window.AddNextRequestor
    constructor: (parent) ->
      @parent = parent

    command_selected: (cmd_type) =>
      new_step = workflow.create_step(cmd_type, @parent)
      workflow.set_as_current(new_step)

  class window.InsertBeforeRequestor
    constructor: (step) ->
      @step = step
      @excluded_types = [Goto, HangUp]

    command_selected: (cmd_type) =>
      parent = @step.parent()
      new_step = workflow.create_step cmd_type, parent, (step) =>
        step.next_id = @step.id
        step.root = @step.root
        @step.root = false
        workflow.set_as_current(step)

  class window.InsertAfterRequestor
    constructor: (step) ->
      @step = step
      @excluded_types = [Goto, HangUp]

    command_selected: (cmd_type) =>
      next_id = @step.next_id
      new_step = workflow.create_step cmd_type, @step, (new_step) =>
        new_step.next_id = next_id
        workflow.set_as_current(new_step)

