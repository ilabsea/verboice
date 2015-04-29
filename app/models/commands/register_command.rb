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

class Commands::RegisterCommand < Command

  def initialize number, reminder_group
    @number = number
    @reminder_group = reminder_group
  end

  def serialize_parameters
    {
      number: @number,
      reminder_group: @reminder_group
    }
  end

  def run(session)
    session.info "Register caller into reminder phone book", command: 'register', action: 'start'
    register_caller_to_reminder_group session
    super
  end

  private

  def register_caller_to_reminder_group session
    reminder_group = session.project.ext_reminder_groups.where(:name => @reminder_group).first
    raise "#{session[:current_step_name]} step is broken" if reminder_group.nil?
    # lookup for caller number
    number = session.eval(@number) || session.address
    session.info "Registering #{number} to #{reminder_group.name}", command: 'register', action: 'registering'
    reminder_group.register_address(number)
    session.info "Registration complete", command: 'register', action: 'finish'
  end

end
