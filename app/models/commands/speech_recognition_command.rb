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

class Commands::SpeechRecognitionCommand < Command

  attr_accessor :filename, :stop_keys, :timeout

  def initialize key, description, options = {}
    @key         = key
    @description = description
    @options = options
  end

  def serialize_parameters
    {
      key: @key,
      description: @description,

      stop_keys: stop_keys,
      timeout:   timeout ,
      silence_detection: silence_detection,
      min_confidence: @options[:min_confidence],

      old_store:   @options[:old_store],
      store:       @options[:store],

      old_result1: @options[:old_result1],
      old_result2: @options[:old_result2],
      old_result3: @options[:old_result3],

      old_accuracy1: @options[:old_accuracy1],
      old_accuracy2: @options[:old_accuracy2],
      old_accuracy3: @options[:old_accuracy3],

      result1: @options[:result1],
      result2: @options[:result2],
      result3: @options[:result3],

      accuracy1: @options[:accuracy1],
      accuracy2: @options[:accuracy2],
      accuracy3: @options[:accuracy3]
    }
  end

  def stop_keys
    @options[:stop_keys] || '01234567890*#'
  end

  def silence_detection  
    @options[:silence_detection].try(:to_i) || 0
  end

  def timeout
    @options[:timeout].try(:to_i) || 10
  end

  def self.default_min_confidence
    "60"
  end

  def self.default_number_of_attempts
    5
  end

end
