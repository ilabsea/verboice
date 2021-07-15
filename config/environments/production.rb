# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

Verboice::Application.configure do
  verboice_config = YAML::load_file("#{Rails.root}/config/verboice.yml").with_indifferent_access
  # Settings specified here will take precedence over those in config/application.rb

  # Code is not reloaded between requests
  config.cache_classes = true

  # Full error reports are disabled and caching is turned on
  config.consider_all_requests_local       = false
  config.action_controller.perform_caching = true

  # Disable Rails's static asset server (Apache or nginx will already do this)
  config.serve_static_assets = false

  # Compress JavaScripts and CSS
  config.assets.compress = true

  # Don't fallback to assets pipeline if a precompiled asset is missed
  config.assets.compile = true

  # Generate digests for assets URLs
  config.assets.digest = true

  # Default url for action mailer
  # config.action_mailer.default_url_options = verboice_config[:default_url_options].symbolize_keys
  config.action_mailer.asset_host = verboice_config[:asset_host]

  config.action_mailer.default_url_options = { host: ENV['SETTINGS__SMTP__HOST'] }

  smtp_settings = {}.tap do |settings|
    settings[:address]              = ENV['SETTINGS__SMTP__ADDRESS'] if ENV['SETTINGS__SMTP__ADDRESS'].present?
    settings[:port]                 = ENV['SETTINGS__SMTP__PORT'].to_i if ENV['SETTINGS__SMTP__PORT'].present?
    settings[:domain]               = ENV['SETTINGS__SMTP__DOMAIN'] if ENV['SETTINGS__SMTP__DOMAIN'].present?
    settings[:user_name]            = ENV['SETTINGS__SMTP__USER_NAME'] if ENV['SETTINGS__SMTP__USER_NAME'].present?
    settings[:password]             = ENV['SETTINGS__SMTP__PASSWORD'] if ENV['SETTINGS__SMTP__PASSWORD'].present?
  end

  if smtp_settings.present?
    config.action_mailer.delivery_method = :smtp
    config.action_mailer.smtp_settings = smtp_settings
  end

  # Send notification when has exception happen
  config.middleware.use ExceptionNotification::Rack,
    :email => {
      :email_prefix => "[Verboice] ",
      :sender_address => %{"notifier" <#{ENV['MAILER_SENDER']}>},
      :exception_recipients => ENV['EXCEPTION_RECIPIENTS'].to_s.split(',')
    }

  # Defaults to Rails.root.join("public/assets")
  # config.assets.manifest = YOUR_PATH

  # Specifies the header that your server uses for sending files
  config.action_dispatch.x_sendfile_header = "X-Sendfile" # for apache
  # config.action_dispatch.x_sendfile_header = 'X-Accel-Redirect' # for nginx

  # Force all access to the app over SSL, use Strict-Transport-Security, and use secure cookies.
  # config.force_ssl = true

  # See everything in the log (default is :info)
  # config.log_level = :debug

  # Use a different logger for distributed setups
  # config.logger = SyslogLogger.new

  # Use a different cache store in production
  # config.cache_store = :mem_cache_store

  # Enable serving of images, stylesheets, and JavaScripts from an asset server
  # config.action_controller.asset_host = "http://assets.example.com"

  # Precompile additional assets (application.js, application.css, and all non-JS/CSS are already added)
  # config.assets.precompile += %w( flow.js )

  # Disable delivery errors, bad email addresses will be ignored
  # config.action_mailer.raise_delivery_errors = false

  # Enable threaded mode
  # config.threadsafe!

  # Enable locale fallbacks for I18n (makes lookups for any locale fall back to
  # the I18n.default_locale when a translation can not be found)
  config.i18n.fallbacks = true

  # Send deprecation notices to registered listeners
  config.active_support.deprecation = :notify
end
