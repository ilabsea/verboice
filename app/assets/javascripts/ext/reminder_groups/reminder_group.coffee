onReminderGroups ->
  class @VariableModel
    constructor: (obj={}) ->
      @source = ko.observable obj.source
      @destination = ko.observable obj.destination

  class @ReminderGroup
    constructor: (data) ->
      @id = ko.observable data?.id
      @name = ko.observable data?.name
      @contacts = ko.observableArray if data?.reminder_group_contacts then $.map(data.reminder_group_contacts, (x) -> new Contact({id: x.id, address: x.address})) else []
      @reminder_group_contacts = data?.reminder_group_contacts || []
      @contacts_display = ko.computed => if @contacts().length == 0 then "No contact" else $.map(@contacts(), (x) -> x.address() if x.valid()).join(", ")
      @has_contacts = ko.computed => if @contacts().length > 0 then true else false
      @new_address = ko.observable null
      @updated_at = ko.observable data?.updated_at

      @mode = ko.observable(data?.mode)
      @enable_sync = ko.observable(data?.enable_sync)

      @endpoint = ko.observable(data?.sync_config.endpoint)
      @username = ko.observable(data?.sync_config.username)
      @password = ko.observable(data?.sync_config.password)
      @schedule = ko.observable(data?.sync_config.schedule)
      @phone_number_field = ko.observable(data?.sync_config.phone_number_field)
      @loading = ko.observable(false)
      @sync_status_updated_at = ko.observable(data?.sync_status_updated_at)

      # Variables
      @variables = ko.observableArray()
      if !!data?.sync_config.variables && data?.sync_config.variables.length > 0
        i = 0
        while i < data?.sync_config.variables.length
          @variables.push(data?.sync_config.variables[i])
          i++

      @name_error = ko.computed => if @has_name() then null else "Name is required"
      @name_duplicated = ko.computed => if @has_name() and @name_exists(@name()) then true else false
      @new_address_duplicated = ko.computed => if @has_new_address() and @address_exists(@new_address()) then true else false
      @new_address_button_disabled = ko.computed => !@has_new_address() or @has_new_address() and @address_exists(@new_address())

      @hasFocus = ko.observable(false)

      @error = ko.computed =>
        isError = @name_error() || @name_duplicated()
        if @mode() == 'sync_with_go_data'
          isError ||= !@endpoint() || !@username() || !@password() || !@phone_number_field()
        isError

      @valid = ko.computed =>
        !@error()

      @collapse_expand_import_state = ko.observable false
      @showMore = ko.computed => !!(@contacts().length > 20)

    addVariable: () =>
      @variables.push(new VariableModel())

    removeVariable: (variable) =>
      @variables.remove(variable)

    copyToClipboard: () =>
      input = document.createElement('input')
      value = @contacts().map (x) -> x.address()
      input.setAttribute 'value', value
      document.body.appendChild input
      input.select()
      document.execCommand 'copy'
      document.body.removeChild input

      $('.alert').show();
      setTimeout (->
        $('.alert').fadeOut 'slow'
        return
      ), 1500

    collapse_expand_import: () =>
      @collapse_expand_import_state(!@collapse_expand_import_state())
      if @collapse_expand_import_state()
        $.each model.reminder_groups(), (index, r) =>
          if r.id() != @id()
            r.collapse_expand_import_state(false)

        $(".ux-collapsible.file_upload").addClass("collapsed")
        $forms = $("form")
        $.each $forms, (index, form) =>
          $form = $(form)
          $form.find("#file_name").val("")

    upload: () =>
      $forms = $("form")
      $.each $forms, (index, form) =>
        $form = $(form)
        if $.trim($form.find("#file_name").val()).length > 0
          action = "/ext/projects/" + project_id + "/reminder_groups/" + @id() + "/import"
          $form.attr("action", action)
          $form.submit()

    sync_reminder_group: () =>
      @loading(true)
      $.ajax
        url: "/api/projects/" + project_id + "/reminder_groups/" + @id() + "/sync_now"
        type: 'PUT'
        data: JSON.stringify({})
        contentType: 'application/json'
        success: (data)=>
          @contacts.removeAll()
          addresses = $.map(data.reminder_group_contacts, (x) -> new Contact({id: x.id, address: x.address}))
          @contacts.push.apply(@contacts, addresses)
          @loading(false)

    cancel_upload: () =>
      $.each model.reminder_groups(), (index, r) =>
        r.collapse_expand_import_state(false)

      $(".ux-collapsible.file_upload").addClass("collapsed")
      $forms = $("form")
      $.each $forms, (index, form) =>
        $form = $(form)
        $form.find("#file_name").val("")

    has_name: => $.trim(@name()).length > 0

    name_exists: (name) =>
      $.map(model.reminder_groups(), (x) => x if x.id() and x.id() != @id() and x.name() == name).length > 0

    has_new_address: => $.trim(@new_address()).length > 0

    address_exists: (address) =>
      ref = @address_matched
      $.map(@contacts(), (x) -> x if ref(x.address(), address)).length > 0

    address_matched: (address, new_address) =>
      @address_without_prefix(address) == @address_without_prefix(new_address)

    address_without_prefix: (address) =>
      prefixes = ["855", "+855", "0", "+0", "+"]
      i = 0
      while i < prefixes.length
        prefix = prefixes[i]
        return address.substr(prefix.length)  if address.substr(0, prefix.length) is prefix
        i++
      address

    add_new_contact: =>
      if @has_new_address()
        unless @address_exists(@new_address())
          contact = new Contact({address: @new_address()})
          @contacts.push contact
          @new_address(null)

    remove_contact: (contact) =>
      @contacts.remove(contact)

    remove_all_contact: =>
      @contacts.removeAll()

    toJSON: =>
      obj =
        name: @name()
        mode: @mode()

      if @mode() == 'sync_with_go_data'
        obj = Object.assign(obj,
          enable_sync: @enable_sync()
          sync_config:
            endpoint: @endpoint()
            username: @username()
            password: @password()
            schedule: @schedule()
            phone_number_field: @phone_number_field()
            variables: @variables().filter (x) -> (typeof x.source == 'string' && !!x.source) && (typeof x.destination == 'string' && !!x.destination)
        )
      else
        obj = Object.assign(obj,
          reminder_group_contacts_attributes: @_buildReminderGroupContactAttrs()
        )

      obj

    _buildReminderGroupContactAttrs: =>
      addresses = $.map(@contacts(), (x) -> x.address() if x.valid())

      i = 0
      while i < @reminder_group_contacts.length
        if addresses.findIndex( (ad) => ad == @reminder_group_contacts[i].address) < 0
          @reminder_group_contacts[i]._destroy = '1'
        else
          @reminder_group_contacts[i]._destroy = '0'
        i++

      i = 0
      while i < addresses.length
        if @reminder_group_contacts.findIndex((x) => x.address == addresses[i]) < 0
          @reminder_group_contacts.push({id: '', address: addresses[i]})
        i++

      @reminder_group_contacts

    exceed_contact_limited: =>
      @contacts().length > 1000 && @mode() == 'manual'
