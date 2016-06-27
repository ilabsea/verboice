InsteddTelemetry.setup do |config|

  config_path = File.join(Rails.root, 'config', 'telemetry.yml')
  custom_config = File.exists?(config_path) ? YAML.load_file(config_path).with_indifferent_access : {}

  config.server_url           = custom_config[:server_url]                   if custom_config.include? :server_url
  config.period_size          = custom_config[:period_size_hours].hours      if custom_config.include? :period_size_hours
  config.process_run_interval = custom_config[:run_interval_minutes].minutes if custom_config.include? :run_interval_minutes
  config.application          = (custom_config.include? :application) ? custom_config[:application] : 'verboice'

  config.api_port = 8089
  config.remote_api_enabled = true

  # Verboice custom collectors

  config.add_collector Telemetry::CallersPerCountryCodeCollector
  config.add_collector Telemetry::CallFlowsPerProjectCollector
  config.add_collector Telemetry::CallsPerDayPerChannelCollector
  config.add_collector Telemetry::LanguagesPerProjectCollector
  config.add_collector Telemetry::ProjectCountCollector
  config.add_collector Telemetry::StepsPerCallFlowCollector
  config.add_collector Telemetry::ActiveChannelsCollector
end
