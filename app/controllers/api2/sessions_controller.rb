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
class Api2::SessionsController < Api2Controller
  skip_before_filter :authenticate_account_from_token!, only: [:create]
  before_filter :ensure_params_exist

  def create

    account = Account.find_by_email(params[:account][:email])

    if account && account.valid_password?(params[:account][:password])

      if account.has_access_from? origin_host
        render :json=> {:success=>true, :auth_token=>account.auth_token, :email=> account.email}
      else
        response_with_not_authorized
      end

    else
      response_with_invalid_credential
    end

  end

  protected

  def ensure_params_exist
    return unless params[:account].blank?
    render :json=>{:success=>false, :message=>"missing user_login parameter"}, :status=>422
  end
 
  def response_with_invalid_credential
    render :json=> {:success=>false, :message=>"Error with your login or password"}, :status=>401
  end
end
