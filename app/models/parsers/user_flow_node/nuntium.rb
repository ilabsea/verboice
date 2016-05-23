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
    class Nuntium < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      QST_SERVER = "qst_server"
      SMTP = "smtp"

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @kind = params['kind'] || QST_SERVER
        @subject = Resource.new params['subject']
        @resource = Resource.new params['resource']
        @recipient = params['recipient']
        # ensure channel belongs to account
        @channel_id = call_flow.account.nuntium_channels.find_by_id(params['channel_id']).try(:id)
        @call_flow = call_flow
        @next = params['next']
        @root_index = params['root']
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
          compiler.StartUserStep :nuntium, @id, @name
          if @resource.guid
            if @recipient['caller']
              compiler.Nuntium @kind, @channel_id, rcpt_type: :caller, resource_guid: @resource.guid
            else
              options = {rcpt_type: :expr, expr: InputSetting.new(@recipient).expression(), resource_guid: @resource.guid}
              options[:subject_guid] = @subject.guid if @kind == SMTP
              compiler.Nuntium @kind, @channel_id, options
            end
          end
          compiler.Trace context_for '"Sent text message."'
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
