class Account::RegistrationsController < Devise::RegistrationsController

  def new
    build_resource
  end

  def create
    if !verify_captcha
      flash[:notice] = "Invalid captcha"
      redirect_to action: :new, account: params[:account]
    else
      super
    end
  end

end