# NOTE: stub_env is originally from [https://gist.github.com/jordanmaguire/1018290]

def stub_env(new_env, &block)
  original_env = Rails.env
  Rails.instance_variable_set("@_env", ActiveSupport::StringInquirer.new(new_env))
  reload_settings by_pass_validate_domain: true
  block.call
ensure
  Rails.instance_variable_set("@_env", ActiveSupport::StringInquirer.new(original_env))
  reload_settings
end

private
  def reload_settings options = {}
    Settings.reload_from_files(
      Rails.root.join("config", "settings.yml").to_s,
      Rails.root.join("config", "settings", "#{Rails.env}.yml").to_s,
      Rails.root.join("config", "environments", "#{Rails.env}.yml").to_s
    )

    # NOTE: by-passs domain validation
    Settings.channel.validate_domain = false if options[:by_pass_validate_domain]
  end
