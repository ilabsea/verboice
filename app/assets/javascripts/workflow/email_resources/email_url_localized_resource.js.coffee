#= require workflow/email_resources/email_localized_resource

onWorkflow ->
  class window.EmailUrlLocalizedResource extends EmailLocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @label = 'Online resource'
      @template = 'url_localized_resource_template'
      @url = ko.observable decodeURI(hash.url or '')

      @is_valid = ko.computed =>
        @url()? and @url().length > 0

    to_hash: =>
      $.extend(super,
        url: @url()
      )

    type: () =>
      'UrlLocalizedResource'
