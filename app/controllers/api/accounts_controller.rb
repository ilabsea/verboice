module Api
  class AccountsController < ApiController
    def index
      if current_account.admin?
        render json: Account.all, each_serializer: CustomAccountSerializer if current_account.admin?  
      else
        head :not_found
      end
    end
  end
end