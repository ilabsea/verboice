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

class Commands::DialCommand < Command
  attr_accessor :number, :channel_name, :caller_id, :successful_after, :record_call, :key, :description

  def initialize(number, options = {})
    @number = number
    @channel_name = options[:channel]
    @caller_id = options[:caller_id]
    @successful_after = options[:successful_after]
    @record_call = options[:record_call]
    @key = options[:key]
    @description = options[:description]
  end

  def serialize_parameters
    params = {number: @number}
    params[:channel_name] = @channel_name if @channel_name.present?
    params[:caller_id] = @caller_id if @caller_id.present?
    params[:successful_after] = @successful_after if @successful_after.present?
    params[:record_call] = true if @record_call
    params[:key] = @key if @key.present?
    params[:description] = @description if @description.present?
    params
  end
end
