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

class Api2Controller < ActionController::Base
  before_filter :authenticate_account_from_token!

  def errors_to_json(model_object, action)
    attrs = {
      :summary => I18n.t("controllers.api_controller.problem_action", :action => action, :model_name => model_object.class.model_name),
      :properties => []
    }
    model_object.errors.each do |name, value|
      attrs[:properties] << { name => value }
    end
    attrs
  end

  def api_current_account 
    return @current_account if @current_account 
    account  = Account.find_by_email(params[:email])

    p "account: "
    p account
    p "token: #{params[:token]}"
 
    if account && account.auth_token == params[:token]
      @current_account = account
    end
    @current_account

  end

  def api_user_signed_in? 
    api_current_account
  end

  def authenticate_account_from_token!
    if !api_user_signed_in?
      response_with_unauthorized
      return false
    end
  end

  def authorize_admin
    return head :unauthorized if !api_admin?
  end

  protected

  def response_with_unauthorized
    render :json=> {:success=>false, :message=>"Not authorized access"}, :status=>401
  end

  def origin_host
    request.remote_ip
  end

  def api_admin?
    api_current_account.admin? && api_current_account.has_access_from?(origin_host)
  end
  
end
