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

require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Deregister do

      let(:call_flow) { CallFlow.make }

      it "should compile to an equivalent flow" do
        step = Deregister.new call_flow, 'id' => 1,
          'reminder_group' => 'pregnancy',
          'name' => 'Deregister Step',
          'confirmation_resource' => {
            "guid" => 2
          }

        step.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.AssignValue "current_step", 1
            c.AssignValue "current_step_name", "Deregister Step"
            c.Deregister "pregnancy"
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Deregister Step', store:'"Deregister contact from pregnancy."'
            c.PlayResource 2
          end.first
        )
      end

    end
  end
end
