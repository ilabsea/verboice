module AppConfigHelper
  def app_configs
  	 # @_app_config ||= YAML.load_file("#{Rails.root}/config/app_config.yml")[Rails.env]
  	 APP_CONFIGS
  end

  def allow_prefix_called_number?
  	app_configs["allow_prefix_called_number"]
  end

end