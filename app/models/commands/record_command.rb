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

class Commands::RecordCommand < Command

  attr_accessor :filename, :stop_keys, :timeout

  def initialize key, description, options = {}
    @key                = key
    @description        = description
    @stop_keys          = options[:stop_keys] || '01234567890*#'
    @silence_detection  = options[:silence_detection].try(:to_i) || 0

    @timeout            = options[:timeout].try(:to_i) || 10
    @old_var_name       = options[:old_var_name]
    @var_name           = options[:var_name]
  end

  def serialize_parameters
    {
      key: @key,
      description: @description,
      stop_keys: @stop_keys,
      timeout: @timeout,
      silence_detection: @silence_detection,
      old_var_name: @old_var_name,
      var_name: @var_name
    }
  end

  def run(session)
    session.info "Record user voice", command: 'record', action: 'start'
    session.pbx.record filename(session), stop_keys, timeout
    session.trace "Recording complete", command: 'record', action: 'complete'
    session.trace "Saving recording", command: 'record', action: 'save'
    create_recorded_audio(session)
    session.info "Recording saved", command: 'record', action: 'finish'
    super
  end

  private

  def filename(session)
    RecordingManager.for(session.call_log).result_path_for(@key)
  end

  def create_recorded_audio(session)
    contact = session.contact
    call_log = session.call_log
    session.trace "Caller address is unknown. Recording '#{@description}' will be saved for contact #{contact.first_address}.", command: 'record', action: 'contact_unknown' unless session.address.presence
    contact.recorded_audios.create! :call_log => call_log, :key => @key, :description => @description

    unless @old_var_name.nil? and @var_name.nil?
      project = call_log.project
      # update old variable name to new name or create a new one
      project_variable = project.project_variables.where(:name => @old_var_name).first
      if project_variable
        project_variable.name = @var_name
        project_variable.save
      else
        project_variable = project.project_variables.where(:name => @var_name).first_or_create
      end
      
      call_log_record_audio = CallLogRecordedAudio.find_by_call_log_id_and_project_variable_id(call_log.id, project_variable.id)
      unless call_log_record_audio
        call_log.call_log_recorded_audios.create! :project_variable_id => project_variable.id, :key => @key, :description => @description if project_variable
      else      
        call_log_record_audio.update_attributes({:project_variable_id => project_variable.id, :key => @key, :description => @description})
      end
      
      # call_log.call_log_recorded_audios.create! :project_variable_id => project_variable.id, :key => @key, :description => @description if project_variable
    end
  end
end
