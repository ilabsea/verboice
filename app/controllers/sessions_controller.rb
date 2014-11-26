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

class SessionsController < Devise::SessionsController

  def new
    ip = Login::Ip.new(origin_ip)
    email = params[:account].nil? ? nil : params[:account][:email]
    @captcha = !email.nil? && ip.reaches_maximum_failed_attempt?(email) ? true : false
    build_resource
  end

  def create
    allow_to_login = true
    ip = Login::Ip.new(origin_ip)
    if ip.reaches_maximum_failed_attempt?(params[:account][:email])
      if !verify_captcha
        allow_to_login = false

        flash[:error] = I18n.t("activerecord.errors.models.login.invalid_captcha")
      end
    end

    if allow_to_login
      if current_account.nil?
        response_with_login_failed(ip)

        flash[:error] = I18n.t("activerecord.errors.models.login.invalid_email_password")
      else
        ip.log(params[:account][:email], LoginTracker::MARKED_AS_SUCCESS)
        
        super
      end
    else
      response_with_login_failed(ip)
    end
  end
  
  private
  def response_with_login_failed(ip)
    ip.log(params[:account][:email], LoginTracker::MARKED_AS_FAILED)

    sign_out current_account if current_account

    redirect_to action: :new, account: params[:account].except(:password)
  end

end