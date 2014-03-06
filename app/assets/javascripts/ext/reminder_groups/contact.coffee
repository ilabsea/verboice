onReminderGroups ->
	class @Contact
		constructor: (data) ->
			@id = ko.observable data?.id
			@address = ko.observable data?.address

			@address_error = ko.computed => if @has_address() then null else "Address is required"

		has_address: => $.trim(@address()).length > 0

		error: => 
			@address_error()

		valid: =>
			!@error()

		toJSON: =>
			address: @address()


			