module Ext
	module ReminderScheduleHelper
		def date_time_calendar f, model, label_text, text_field, text_value, html_options_label = {}, html_options_text = {}
			html_options_label["class"] = html_options_label["class"].nil? ? "inline" : " inline #{html_options_label["class"]}" 
			html_options_text["class"] = html_options_text["class"].nil? ? "datetimepicker w30" : " datetimepicker w30 #{html_options_label["class"]}" 
			
			f.label(text_field, label_text, :class => "inline") + 
			text_field_tag(ActiveModel::Naming.param_key(model)+"[#{text_field}]" , text_value , :class => "datetimepicker w30") 
		end
	end


end