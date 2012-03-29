# var commands = {"answer":[],"callback":[{"name":"url","type":"string","optional":true},{"name":"method","type":"string","optional":true,"default":"post"}],"capture":[{"name":"min","type":"integer","default":1,"ui_length":1},{"name":"max","type":"integer","default":1,"ui_length":1},{"name":"finish_on_key","type":"string","default":"#","ui_length":1},{"name":"timeout","type":"integer","default":5,"ui_length":1},{"name":"play","type":"string","ui_length":40},{"name":"say","type":"string","ui_length":40}],"dial":[{"name":"number","type":"string","ui_length":80},{"name":"channel","type":"string","ui_length":20}],"hangup":[],"pause":[{"name":"length","type":"integer","default":1,"ui_length":3}],
# "play_url":[{"name":"url","type":"string","ui_length":80}],"record":[],"say":[{"name":"text","type":"string","ui_length":80}]};
# var flow = ["hangup",{"play_url":""},{"capture":{"min":"1","max":"1","finish_on_key":"#","timeout":"5","play":"","say":""}},{"play_url":""}];

# TODO: Change the array representation to a linked list to support the 'if' command

jQuery ->
  if not $('#workflow').length > 0
    return


  class WorkflowDrawer
    constructor: (container) ->
      @container = $(container)
      
    draw_workflow: (steps) =>
      @matrix_yx = []
      @container.empty()
      y = 0
      roots = (step for step in steps when step.root)
      for root in roots
        [_x,y] = @recursive_draw_workflow(root, 0, y)
      @draw_matrix()
    
    recursive_draw_workflow: (step, x, y, klass='ha') =>
      @set_step(step, x, y, klass)
      [next_x, next_y] = [x+1, y]
      klass = 'ha'
      for child in step.children()
        [_x, next_y] = @recursive_draw_workflow(child, next_x, next_y, klass)
        klass = 'va'
      next_y = y+1 if next_y < y+1
      return [x, next_y]
    
    set_step: (step, x, y, klass) =>
      @matrix_yx[y] ?= []
      for x_i in [0..x-1]
        if not @matrix_yx[y][x_i]?
          @matrix_yx[y][x_i] = [false, klass]
      @matrix_yx[y][x] = [step, klass]

    draw_matrix: () =>
      for row in @matrix_yx
        @draw_newline()
        for [elem, klass] in row
          if elem == false
            @draw_empty()
          else
            @draw_step(elem, klass)

    draw_newline: () =>
      @container.append('<p> </p>')
    
    draw_empty: () =>
      @container.append('<div> </div>')
    
    draw_step: (step, klass="") =>
      @container.append("<div class=\"#{klass}\" data-bind=\"template: { name: '#{step.item_template_id()}', data: get_step(#{step.id}) }\"> </div>")

  
  ko.bindingHandlers.workflow_steps =
    init: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      container = $("<div class='workflow-container'></div>").appendTo(element)
      viewModel.workflow_drawer = new WorkflowDrawer(container)
    update: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      steps = ko.utils.unwrapObservable(valueAccessor())
      viewModel.workflow_drawer.draw_workflow(steps)





  class Workflow
    constructor: (command_selector) ->
      @steps = ko.observableArray(Step.from_hash(hash) for hash in application_flow)
      @command_selector = ko.observable(command_selector)
      @current_step = ko.observable @command_selector()

    get_step: (id) =>
      (step for step in @steps() when step.id == id)[0]

    add_step: (command) =>
      @steps.push command
      # @steps.push Step.from_command(command)

    remove_step: (step) =>
      @steps.remove(step)
      @initialize_current_step()

    set_as_current: (step) =>
      @current_step(step)

    initialize_current_step: () =>
      @set_as_current(@command_selector())

    display_template_for: (current_flow_step) =>
      @current_step().display_template_id()

    # Persist change on the server
    submitChange: () =>
    #   $.ajax {
    #     type: 'POST',
    #     url: update_workflow_application_path,
    #     data: {
    #       _method: 'PUT',
    #       flow: @flow_array()
    #     },
    #     success: (data) ->
    #       window.location = application_path;
    #     dataType: 'json'
    #   }

    # flow_array: () =>
    #   output = new Array
    #   for step in @steps()
    #     args = new Object
    #     flow_step= new Object

    #     for arg in step.arguments()
    #       args[arg.name()] = arg.value()

    #     flow_step[step.name()] = args
    #     output.push(flow_step)
    #   output

  class CommandSelector
    constructor: ->
      handlers = (new ClassBindingHandler(klass) for klass in [Menu])
      @commands = ko.observableArray(handlers)

    # command_named: (name) =>
    #   (command for command in @commands() when command.name() is name)[0]

    display_template_id: () ->
      'command_selector_template'

    add_menu_to_steps: () ->
      workflow.add_step(new Menu)

  class ClassBindingHandler
    constructor: (cmd)->
      @cmd = cmd
    add_to_steps: =>
      @cmd.add_to_steps()

  class Step
    constructor: () ->
      @root = false
    
    @from_hash: (hash) ->
      item = null
      switch hash['type']
        when 'menu'
          item = Menu.from_hash(hash)
        else
          throw "Command type not recognised #{hash['type']}"
      
      item.root = hash['root']
      item.id = hash['id']
      return item
    
    is_current_step: () =>
      workflow.current_step == @

    remove: () =>
      workflow.remove_step @

    set_as_current: () =>
      workflow.set_as_current @
    
    children: () =>
      (step for step in workflow.steps() when step.id in @next_ids())
    
    item_template_id: () =>
      'workflow_step_template'
    

  class Menu extends Step
    constructor: (name, options) ->
      @name = ko.observable name || 'Menu'
      @options = ko.observableArray(options)

    display_template_id: () =>
      'menu_step_template'

    button_class: () =>
      'ldial'

    start_recording: () =>
      Wami.startRecording(save_recording_application_path);

    stop_recording: () =>
      Wami.stopRecording()

    next_ids: () =>
      (option.next() for option in @options())
    
    @add_to_steps: () ->
      workflow.add_step(new Menu)
      
    @from_hash: (hash) ->
      options = (new MenuOption(opt.number, opt.description, opt.next) for opt in (hash.options || []))
      menu = new Menu(hash['name'], options)
      return menu
      
  
  class MenuOption
    constructor: (number, description, next) ->
      @number = ko.observable number
      @description = ko.observable description
      @next = ko.observable next


  ko.bindingHandlers['class'] = {
    'update': (element, valueAccessor) ->
      if (element['__ko__previousClassValue__'])
          $(element).removeClass(element['__ko__previousClassValue__'])
      value = ko.utils.unwrapObservable(valueAccessor())
      $(element).addClass(value)
      element['__ko__previousClassValue__'] = value
  }
  
  workflow = new Workflow(new CommandSelector)
  ko.applyBindings(workflow)

  Wami.setup {id: 'wami', swfUrl: '/Wami.swf'}

  # Wami.startPlaying(anyWavURL);
  # Wami.stopPlaying();