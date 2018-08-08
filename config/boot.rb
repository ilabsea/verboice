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

require 'rubygems'
require 'yaml'

# Set up gems listed in the Gemfile.
ENV['BUNDLE_GEMFILE'] ||= File.expand_path('../../Gemfile', __FILE__)

require 'bundler/setup' if File.exists?(ENV['BUNDLE_GEMFILE'])
require File.expand_path('../initializers/date_comparison',  __FILE__)

module Options
  extend self

  CONFIG = YAML.load_file(File.expand_path('../settings.yml', __FILE__))

  def is_on?(plugin)
    plugins[plugin.to_s] == true
  end

  def selected_plugins
    plugins.map{|k,v| k if v == true }.compact
  end

  def method_missing(method_name)
    if method_name.to_s =~ /(\w+)\?$/
      CONFIG[$1] == true
    else
      CONFIG[method_name.to_s]
    end
  end
end
