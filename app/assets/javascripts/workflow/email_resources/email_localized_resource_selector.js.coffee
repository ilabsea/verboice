#= require workflow/email_resources/email_text_localized_resource
#= require workflow/email_resources/email_url_localized_resource
#= require workflow/email_resources/email_record_localized_resource
#= require workflow/email_resources/email_upload_localized_resource

onWorkflow ->
  class window.EmailLocalizedResourceSelector

    constructor: (options = []) ->
      @options = ko.observableArray options
      @current = ko.observable options[0]
      @title = ko.observable ''
      @language = null

      @is_valid = ko.computed =>
        @current()?.is_valid()
      @is_text = ko.computed =>
        @current()?.type() == "TextLocalizedResource"

    to_hash: () =>
      @current().to_hash()

    @from_hash: (hash) ->
      options = (new window["#{type}LocalizedResource"](hash) for type in ['EmailText'])
      selector = new EmailLocalizedResourceSelector(options)

      for option in options
        selector.current(option) if option.type() == hash.type

      return selector

    with_title: (new_title) =>
      @title(new_title)
      return @

    with_language: (language) =>
      @language = language
      return @

    with_parent: (parent) =>
      for option in @options()
        option.set_parent(parent)
      return @

    id: (id) =>
      for option in @options()
        option.id(id) unless option.id()?
