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

RECAPTCHA_CONFIG = File.exists?("#{Rails.root}/config/recaptcha.yml") ? YAML.load_file("#{Rails.root}/config/recaptcha.yml")[Rails.env] : {}

Recaptcha.configure do |config|
  config.public_key  = RECAPTCHA_CONFIG['public_key']
  config.private_key = RECAPTCHA_CONFIG['private_key']
  config.proxy = RECAPTCHA_CONFIG['proxy']
end