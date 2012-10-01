class ActionView::Helpers::FormBuilder
	alias :orig_label :label

	# add a '*' after the field label if the field is required
	def label(method, content_or_options = nil, options = nil, &block)
		if content_or_options && content_or_options.class == Hash
			options = content_or_options
		else
			content = content_or_options
		end
		options = options || {}

		content ||= method.to_s.humanize
		content = content
		required_mark = ''
		required_text = options[:"field-required"]  || '*'
		required_mark = "<span class='field-required' >#{required_text}</span>" if object.class.validators_on(method).map(&:class).include? ActiveModel::Validations::PresenceValidator
		content = content 

		self.orig_label(method, options ) do
			(content + required_mark).html_safe
		end
	end
end
