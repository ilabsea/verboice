STEP_CONFIG = File.exists?("#{Rails.root}/config/step_config.yml") ? YAML.load_file("#{Rails.root}/config/step_config.yml")[Rails.env] : {}