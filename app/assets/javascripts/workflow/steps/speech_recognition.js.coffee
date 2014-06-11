#= require workflow/steps/step

onWorkflow ->
  class window.SpeechRecognition extends Step
    @type = 'speech_recognition' #template ref id

    constructor: (attrs) ->
      super(attrs)

      @old_store     = ko.observable attrs.store 
      @store         = ko.observable attrs.store 

      @old_result1   = ko.observable attrs.result1
      @old_result2   = ko.observable attrs.result2
      @old_result3   = ko.observable attrs.result3

      @old_accuracy1 = ko.observable attrs.accuracy1
      @old_accuracy2 = ko.observable attrs.accuracy2
      @old_accuracy3 = ko.observable attrs.accuracy3


      @result1   = ko.observable attrs.result1
      @result2   = ko.observable attrs.result2
      @result3   = ko.observable attrs.result3

      @accuracy1 = ko.observable attrs.accuracy1
      @accuracy2 = ko.observable attrs.accuracy2
      @accuracy3 = ko.observable attrs.accuracy3


      @defines_result1   = ko.observable !!attrs.result1
      @defines_accuracy1 = ko.observable !!attrs.accuracy1
      
      @defines_store = ko.observable !!attrs.store

      @min_confidence = ko.observable(attrs.min_confidence ? speech_recognition_default_min_confidence )
      @number_of_attempts = ko.observable(attrs.number_of_attempts ? capture_default_number_of_attempts)
      @timeout = ko.observable (attrs.timeout || '10')
      @silence_detection = ko.observable (attrs.silence_detection || 0)
      @stop_key = ko.observable (attrs.stop_key || '#')

      @current_editing_resource = ko.observable null

      @resources =
        invalid:      new ResourceEditor(@, attrs.invalid_resource)
        instructions: new ResourceEditor(@, attrs.instructions_resource)

      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_instructions_resource_invalid = ko.computed () =>
        not @resources.instructions.is_valid()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_instructions_resource_invalid()

      @is_result1_value_invalid = ko.computed () =>
        not @result1()

      @is_store_value_invalid = ko.computed () =>
        not @store()  

      @is_accuracy1_value_invalid = ko.computed () =>
        not @accuracy1()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_instructions_resource_invalid() or @is_result1_value_invalid() or @is_accuracy1_value_invalid() or @is_store_value_invalid()

    button_class: () =>
      'control_step lspeech_recognition'

    default_name: () =>
      'Speech Recognition'
  
    @add_to_steps: () ->
      workflow.add_step(new SpeechRecognition)

    @initialize: (hash) ->
      step = new SpeechRecognition(hash)
      return step


    to_hash: () =>
      $.extend(super,
        old_store: (if @old_store() then @old_store() else null)

        old_result1: (if @old_result1() then @old_result1() else null)
        old_result2: (if @old_result2() then @old_result2() else null)
        old_result3: (if @old_result3() then @old_result3() else null)

        old_accuracy1: (if @old_accuracy1() then @old_accuracy1() else null)
        old_accuracy2: (if @old_accuracy2() then @old_accuracy2() else null)
        old_accuracy3: (if @old_accuracy3() then @old_accuracy3() else null)

        store: (if @store() then @store() else null)

        result1: (if @result1() then @result1() else null)
        result2: (if @result2() then @result2() else null)
        result3: (if @result3() then @result3() else null)

        accuracy1: (if @accuracy1() then @accuracy1() else null)
        accuracy2: (if @accuracy2() then @accuracy2() else null)
        accuracy3: (if @accuracy3() then @accuracy3() else null)

        min_confidence: @min_confidence()
        number_of_attempts: @number_of_attempts()   
        timeout: @timeout()
        silence_detection: @silence_detection()
        stop_key: @stop_key()
        
        invalid_resource: @resources.invalid.to_hash()
        instructions_resource: @resources.instructions.to_hash()
      )
    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_invalid_resource: () =>
      @show_resource('invalid')

    show_instructions_resource: () =>
      @show_resource('instructions')

    available_keys: () =>
      ['1','2','3','4','5','6','7','8','9','0','#','*']
