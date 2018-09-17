module AuthHelper
  def http_login account
    request.env['HTTP_AUTHORIZATION'] = ActionController::HttpAuthentication::Basic.encode_credentials(account.email, account.password)
  end  
end
