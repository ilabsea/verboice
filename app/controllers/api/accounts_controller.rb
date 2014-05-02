module Api
  class AccountsController < ApiController
    def index
      if !params[:api_key].blank? && current_account.admin?(params[:api_key])
        render json: Account.all, each_serializer: CustomAccountSerializer
      else
        head :not_found
      end
    end
  end
end