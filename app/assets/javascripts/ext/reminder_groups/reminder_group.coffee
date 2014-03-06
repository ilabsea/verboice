onReminderGroups ->
	class @ReminderGroup
		constructor: (data) ->
			@id = ko.observable data?.id
			@name = ko.observable data?.name
			@contacts = ko.observableArray if data?.addresses then $.map(data.addresses, (x) -> new Contact({address: x})) else []
			@contacts_display = ko.computed => if @contacts().length == 0 then "No contact" else $.map(@contacts(), (x) -> x.address() if x.valid()).join(", ")
			@has_contacts = ko.computed => if @contacts().length > 0 then true else false
			@new_address = ko.observable null

			@name_error = ko.computed => if @has_name() then null else "Name is required"
			@name_duplicated = ko.computed => if @has_name() and @name_exists(@name()) then true else false
			@new_address_duplicated = ko.computed => if @has_new_address() and @address_exists(@new_address()) then true else false
			@new_address_button_disabled = ko.computed => !@has_new_address() or @has_new_address() and @address_exists(@new_address())

			@hasFocus = ko.observable(false)

			@error = ko.computed => 
				@name_error() || @name_duplicated()
			@valid = ko.computed => 
				!@error()

			@collapse_expand_import_state = ko.observable false

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
			$.map(@contacts(), (x) -> x if x.address() == address).length > 0

		add_new_contact: =>
			if @has_new_address()
				unless @address_exists(@new_address())
					contact = new Contact({address: @new_address()})
					@contacts.push contact
					@new_address(null)

		remove_contact: (contact) =>
			@contacts.remove(contact)

		toJSON: =>
			name: @name()
			addresses: $.map(@contacts(), (x) -> x.address() if x.valid())
