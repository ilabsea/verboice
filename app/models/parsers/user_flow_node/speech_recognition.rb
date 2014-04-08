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
    class SpeechRecognition < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @call_flow   = call_flow
        @id = params['id']
        @name = params['name'] || ''

        @timeout           = params['timeout']
        @silence_detection = params['silence_detection']
        @stop_key          = params['stop_key']

        @next              = params['next']
        @root_index        = params['root']

        @old_store     = params['old_store']
        @store         = params['store']

        @old_result1   = params['old_result1']
        @old_accuracy1 = params['old_accuracy1']
        
        @old_result2   = params['old_result2']
        @old_accuracy2 = params['old_accuracy2']

        @old_result3   = params['old_result3']
        @old_accuracy3 = params['old_accuracy3']


        @result1   = params['result1']
        @accuracy1 = params['accuracy1']
        
        @result2   = params['result2']
        @accuracy2 = params['accuracy2']
        
        @result3   = params['result3']
        @accuracy3 = params['accuracy3']

        @number_of_attempts    = params['number_of_attempts'] || self.class.default_number_of_attempts
        @min_confidence        = params['min_confidence'] || self.class.default_min_confidence

        @invalid_resource      = Resource.new params['invalid_resource']
        @instructions_resource = Resource.new params['instructions_resource']
        
      end

      def self.default_min_confidence
        Commands::SpeechRecognitionCommand.default_min_confidence
      end

      def self.default_number_of_attempts
        Commands::SpeechRecognitionCommand.default_number_of_attempts
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def valid_min_confidence
        if @min_confidence && !@min_confidence.blank?
           "(confidence1 >= #{@min_confidence}) "
        else
           "(false)"
        end
      end

      def equivalent_flow
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.AssignValue "current_step", @id
          compiler.AssignValue "current_step_name", "#{@name}"
          compiler.Trace context_for %("Speech recognition audio link: " + record_url(#{@id}))
          compiler.AssignValue "attempt_number#{@id}", 1
          compiler.append @instructions_resource.equivalent_flow

          compiler.While "attempt_number#{@id} <= #{@number_of_attempts}" do |compiler|
            compiler.SpeechRecognition @id, @name,{ 
                                             :stop_keys      => @stop_key,
                                             :timeout        => @timeout,
                                             :silence_detection => @silence_detection,
                                             :min_confidence => @min_confidence.to_s,
                                       
                                             :old_store     => @old_store,
                                             :store         => @store, 

                                             :old_result1   => @old_result1, 
                                             :old_result2   => @old_result2, 
                                             :old_result3   => @old_result3,

                                             :old_accuracy1 => @old_accuracy1,
                                             :old_accuracy2 => @old_accuracy2, 
                                             :old_accuracy3 => @old_accuracy3,

                                             :result1       => @result1,
                                             :result2       => @result2,
                                             :result3       => @result3,

                                             :accuracy1     => @accuracy1,
                                             :accuracy2     => @accuracy2,
                                             :accuracy3     => @accuracy3
                                           }

            compiler.If valid_min_confidence do |compiler|
              compiler.Trace context_for '"Speech has been recognized and accepted"'
              compiler.Goto "end#{@id}"
            end

            compiler.Else do |compiler|
              compiler.If "attempt_number#{@id} < #{@number_of_attempts}" do |compiler|
                compiler.append @invalid_resource.equivalent_flow
              end
            end
            compiler.Assign "attempt_number#{@id}", "attempt_number#{@id} + 1"
          end
          compiler.Label "end#{@id}"
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
