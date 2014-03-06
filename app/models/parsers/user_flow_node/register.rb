
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

module Parsers
  module UserFlowNode
    class Register < UserCommand
      REGISTER_CURRENT_DATE = 'CurrentDate'
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @confirmation_resource = Resource.new params['confirmation_resource']
        @call_flow = call_flow
        @next = params['next']
        @root_index = params['root']
        @reminder_group = params['reminder_group']
        @option = params['option']
        @persisted_variable_name = params['store']
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.AssignValue "current_step", @id
          compiler.AssignValue "current_step_name", "#{@name}"
          compiler.Register number, @reminder_group
          compiler.Trace context_for %("Register contact to #{@reminder_group}.")
          compiler.append @confirmation_resource.equivalent_flow
          compiler.PersistVariable @persisted_variable_name, "value_#{@id}", 'CurrentDate' if @persisted_variable_name
          compiler.append @next.equivalent_flow if @next
        end
      end

      def number
        InputSetting.new(@option).expression() unless current_caller
      end

      def current_caller
        return true if @option.nil?
        @option['current_caller'] ? true : false
      end
    end
  end
end
