module AppConfigHelper
  def app_configs
  	 APP_CONFIGS
  end

  def allow_prefix_called_number?
  	app_configs["allow_prefix_called_number"]
  end

end