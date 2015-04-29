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

class Commands::HangupAndCallbackCommand < Command
  def initialize(options = {})
    @dial_prefix = options[:dial_prefix]
    @when = options[:when]
    @delay = options[:delay]
    @selected_call_flow_id = options[:selected_call_flow_id]
    @retries = options[:retries]
  end

  def serialize_parameters
    params = { dial_prefix: @dial_prefix ,  when: @when, delay: @delay}
    if @selected_call_flow_id.present?
      params[:selected_call_flow_id] = @selected_call_flow_id
      params[:retries] = @retries
    end
    params
  end

end
