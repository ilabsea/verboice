# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

module WorkflowHelper

  def store_value_tags_without_checkbox
    content_tag(:span, "Store this result as: ", :for => 'store-this-result-as') +\
    content_tag(:input, '', :type => 'text', 'data-bind' => 'value: store, initAutocomplete: {source: workflow.all_variables()}, initMask: {mask: $.mask.masks.token}', :style => "width: 108px")
  end

  def store_value_tags
    content_tag(:input, '', :type => 'checkbox', 'data-bind' => 'checked: defines_store', :id => 'store-this-result-as') +\
    content_tag(:label, "Store this result as: ", :for => 'store-this-result-as') +\
    content_tag(:input, '', :type => 'text', 'data-bind' => 'value: store, enable: defines_store, initAutocomplete: {source: workflow.all_variables()}, initMask: {mask: $.mask.masks.token}', :style => "width: 108px")
  end

  def required_store_value_tags
    content_tag(:span, '', 'data-bind' => 'css: {alert: is_store_value_invalid}') +\
    content_tag(:label, t('views.call_flows._record_step_template.label.store_this_result_as'), :for => 'store-this-result-as', 'data-bind' => 'css: {orange: is_store_value_invalid}') +\
    content_tag(:input, '', :type => 'text', id: 'store-this-result-as', 'data-bind' => "value: store, initAutocomplete: {source: workflow.all_variables()}, initMask: {mask: $.mask.masks.token}, valueUpdate: 'afterkeydown', css: {error: is_store_value_invalid}", :style => "width: 108px")
  end

  def user_step_class(step_type, error = false)
    icon_class = case step_type
      when "play", "twiml_play", "twiml_say" then "sound"
      when "branch" then "directions"
      when "input", "twiml_gather" then "numeral"
      when "menu" then "dial"
      when "impersonate" then "users"
      when "hangup_and_callback" then "forwardCall"
      when "language" then "language"
      when "record" then "microphone"
      when "hangup", "twiml_hangup" then "phone"
      when "transfer" then "callback"
      else "cloud"
      end
    "i48grad #{icon_class}#{error ? " red" : ""}"
  end

end
