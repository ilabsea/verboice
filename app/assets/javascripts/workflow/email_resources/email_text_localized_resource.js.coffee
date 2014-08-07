#= require workflow/email_resources/email_localized_resource

onWorkflow ->
  class window.EmailTextLocalizedResource extends EmailLocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @label = 'Text to speech'
      @template = 'email_text_localized_resource_template'
      @text = ko.observable hash.text
      @variables = ko.observableArray(
        for index, variable of distinct_variables.sort() when variable not in workflow.variables()
          if parseInt(index) != distinct_variables.length - 1 then "{" + variable + "}," else "{" + variable + "}")

      @is_valid = ko.computed =>
        @text()? and @text().length > 0

    to_hash: =>
      $.extend(super,
        text: @text()
      )

    type: () =>
      'TextLocalizedResource'

    variable_selected: (var_name) =>
      @text(@text() + var_name.replace(",", ""))

    
