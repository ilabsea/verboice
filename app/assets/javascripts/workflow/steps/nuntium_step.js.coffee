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
      @sms_channel_id = ko.observable if @is_sms_channel() then attrs.nuntium_channel_id else null
      @smtp_channel_id = ko.observable if @is_smtp_channel() then attrs.nuntium_channel_id else null
      @recipient = new NuntiumRecipient((if @is_sms_channel() then attrs.recipient) || { caller: true })
      @email_to = new NuntiumRecipient((if @is_smtp_channel() then attrs.recipient) || { value: "" })

      @is_invalid = ko.computed () => @is_name_invalid() or @is_resource_invalid() or @is_sms_configured_invalid() or @is_smtp_configured_invalid()

    is_sms_channel: -> @kind() is "qst_server"
    is_smtp_channel: -> @kind() is "smtp"

    is_sms_configured_invalid: -> if (@is_sms_channel() and (@recipient.is_invalid() or @is_sms_channel_invalid())) then true else false
    is_smtp_configured_invalid: -> if (@is_smtp_channel() and (@email_to.is_invalid() or @is_subject_invalid() or @is_smtp_channel_invalid())) then true else false
    is_sms_channel_invalid: -> not ($.trim(@sms_channel_id()).length > 0)
    is_smtp_channel_invalid: -> not ($.trim(@smtp_channel_id()).length > 0)
    is_nuntium_channel_invalid: -> @is_sms_channel_invalid() or @is_smtp_channel_invalid()

    available_smtp_channels: () =>
      smtp_channels = []
      smtp_channels.push(nuntium_channel) for nuntium_channel in nuntium_channels when (nuntium_channel.kind == 'smtp')
      @smtp_channel_id(smtp_channels[0]) if smtp_channels.length > 0
      smtp_channels

    available_sms_channels: () =>
      sms_channels = []
      sms_channels.push(nuntium_channel) for nuntium_channel in nuntium_channels when (nuntium_channel.kind == 'qst_server')
      @sms_channel_id(sms_channels[0]) if sms_channels.length > 0
      sms_channels

    button_class: () =>
      'lmessage'

    default_name: () =>
      'Send SMS'

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
        nuntium_channel_id: if @is_sms_channel() then @sms_channel_id() else @smtp_channel_id()
      )

    show_resource: () =>
      @current_editing_resource(@resource)

    show_subject: () =>
      @current_editing_resource(@subject)
