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
    describe Register do

      let(:call_flow) { double('call_flow', :id => 5) }

      before(:each) do
        @options = {
          'id' => 1,
          'reminder_group' => 'pregnancy',
          'name' => 'Register Step',
          'confirmation_resource' => {
            "guid" => 2
          }
        }
      end

      context "Current caller" do
        it "should compile to an equivalent flow" do
          @options.merge! 'option' => {'current_caller' => 'current_caller'}
          register = Register.new call_flow, @options

          register.equivalent_flow.first.should eq(
            Compiler.parse do |c|
              c.Label 1
              c.AssignValue "current_step", 1
              c.AssignValue "current_step_name", "Register Step"
              c.Register nil, "pregnancy"
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Register Step', store:'"Register contact to pregnancy."' 
              c.PlayResource 2
            end.first
          )
        end
      end

      context "Other phone number from input step" do
        it "should compile to an equivalent flow" do
          @options.merge! 'option' => {'step' => '9999'}
          register = Register.new call_flow, @options

          register.equivalent_flow.first.should eq(
              Compiler.parse do |c|
                c.Label 1
                c.AssignValue "current_step", 1
                c.AssignValue "current_step_name", "Register Step"
                c.Register "value_9999", "pregnancy"
                c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Register Step', store:'"Register contact to pregnancy."' 
                c.PlayResource 2
              end.first
            )
        end
      end

      context "Other phone number from variable" do
        it "should compile to an equivalent flow" do
          @options.merge! 'option' => {'variable' => 'foo'}
          register = Register.new call_flow, @options

          register.equivalent_flow.first.should eq(
              Compiler.parse do |c|
                c.Label 1
                c.AssignValue "current_step", 1
                c.AssignValue "current_step_name", "Register Step"
                c.Register "var_foo", "pregnancy"
                c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Register Step', store:'"Register contact to pregnancy."' 
                c.PlayResource 2
              end.first
            )
        end
      end
    end
  end
end
