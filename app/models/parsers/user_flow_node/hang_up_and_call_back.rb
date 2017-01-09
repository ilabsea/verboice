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
    class HangUpAndCallBack < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @call_flow = call_flow
        @next = params['next']
        @root_index = params['root']
        @dial_prefix = params['dial_prefix']
        @when = params['when'] || 'immediately'
        @delay = params['delay'] || '1h'
        @selected_call_flow_id = params['selected_call_flow_id']
        @retries  = params['retries']
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def needs_call_to_be_answered?
        false
      end

      def equivalent_flow
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.StartUserStep :hangup_and_callback, @id, @name
          compiler.HangupAndCallback(dial_prefix: @dial_prefix,
            when: @when, delay: @delay,
            selected_call_flow_id: @selected_call_flow_id,
            retries: @retries)

          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
