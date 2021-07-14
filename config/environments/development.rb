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
  # Settings specified here will take precedence over those in config/application.rb

  # In the development environment your application's code is reloaded on
  # every request.  This slows down response time but is perfect for development
  # since you don't have to restart the web server when you make code changes.
  config.cache_classes = false

  # Log error messages when you accidentally call methods on nil.
  config.whiny_nils = true

  # Show full error reports and disable caching
  config.consider_all_requests_local       = true
  config.action_controller.perform_caching = false

  # Default url for action mailer
  config.action_mailer.default_url_options = { host: ENV['SETTINGS__SMTP__HOST'] }
  config.action_mailer.asset_host = 'http://localhost:3000'

  # Don't care if the mailer can't send.
  config.action_mailer.raise_delivery_errors = false

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

  # Print deprecation notices to the Rails logger
  config.active_support.deprecation = :notify

  # Only use best-standards-support built into browsers
  config.action_dispatch.best_standards_support = :builtin

  # Do not compress assets
  config.assets.compress = false

  # Expands the lines which load the assets
  config.assets.debug = true
end

class_reloader = ActiveSupport::FileUpdateChecker.new(Dir.glob("#{Rails.root}/app/**/*.rb")) do
  Rails.application.eager_load!
end

ActionDispatch::Reloader.to_prepare do
  class_reloader.execute_if_updated
end
