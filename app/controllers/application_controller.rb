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

class ApplicationController < ActionController::Base
  protect_from_forgery
  include ActionView::Helpers::TextHelper
  include AppConfigHelper

  helper_method :verify_captcha

  before_filter :set_locale

  before_filter do
    @body_class = ['full-width']
  end

  def set_fixed_width_content
    @body_class << 'fixed-width-content'
  end

  def origin_ip
    request.remote_ip
  end

  private
  def set_locale
    if params[:recaptcha_response_field].nil?
      I18n.locale = current_account.nil? ? I18n.default_locale : current_account.locale
    else
      I18n.locale = I18n.default_locale
    end
  end

  def verify_captcha
    verify_recaptcha(private_key: RECAPTCHA_CONFIG['private_key'])
  end

end
