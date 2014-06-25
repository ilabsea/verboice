#= require workflow/steps/step
#= require workflow/steps/nuntium_recipient

onWorkflow ->
  class window.Nuntium extends Step
    @type = 'nuntium'

    constructor: (attrs) ->
      super(attrs)
      
      @next_id = attrs.next

      @resource =  new ResourceEditor(@, attrs.resource)
      @current_editing_resource = ko.observable(null)
      @is_editing_resource = ko.computed () => @current_editing_resource()?
      @is_resource_invalid = ko.computed () => not @resource.is_valid() or not @resource.is_text()

      @subject = new ResourceEditor(@, attrs.subject)
      @current_editing_subject = ko.observable(null)
      @is_editing_subject = ko.computed () => @current_editing_subject()?
      @is_subject_invalid = ko.computed () => not @subject.is_valid() or not @subject.is_text()

      @kind = ko.observable attrs.kind || "qst_server"
      @recipient = new NuntiumRecipient((if @is_sms_channel() then attrs.recipient) || { caller: true })
      @email_to = new NuntiumRecipient((if @is_smtp_channel() then attrs.recipient) || { value: "" })

      @is_invalid = ko.computed () => @is_name_invalid() or @is_resource_invalid() or @is_sms_configured_invalid() or @is_smtp_configured_invalid()

    is_sms_channel: -> @kind() is "qst_server"
    is_smtp_channel: -> @kind() is "smtp"

    is_sms_configured_invalid: -> if (@is_sms_channel() and @recipient.is_invalid()) then true else false
    is_smtp_configured_invalid: -> if (@is_smtp_channel() and (@email_to.is_invalid() or @is_subject_invalid())) then true else false

    button_class: () =>
      'lmessage'

    default_name: () =>
      'Send Message'

    @add_to_steps: () ->
      workflow.add_step(new Nuntium)

    @initialize: (hash) ->
      step = new Nuntium(hash)
      return step

    to_hash: () =>
      $.extend(super,
        kind: @kind(),
        recipient: if @is_sms_channel() then @recipient.to_hash() else @email_to.to_hash(),
        subject: @subject.to_hash(),
        resource: @resource.to_hash(),
      )

    show_resource: () =>
      @current_editing_resource(@resource)

    show_subject: () =>
      @current_editing_resource(@subject)
