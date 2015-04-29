class RegistrationsController < Devise::RegistrationsController

  def new
    build_resource
  end

  def edit
  end

  def create
    if !verify_captcha
      flash[:error] = I18n.t("activerecord.errors.models.login.invalid_captcha")
      redirect_to action: :new, account: params[:account].except!(:password, :password_confirmation)
    else
      super
    end
  end

end