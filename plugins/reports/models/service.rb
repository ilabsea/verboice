class Service
	
  def self.from_voice_to_speech audio_file, call_id
    speech = Speech.new(Reports::Settings.google_api_key)

    log = CallLog.find call_id
    file_audio = RecordingManager.for(log).result_path_for(audio_file)
    
    result = speech.recognize file_audio, Reports::Settings.language_recognition
    
    return result['transcript']
  end

  def self.from_speech_to_understanding message
    wit = Wit.new(access_token: Reports::Settings.wit_token)
    json_response = wit.message message
    return json_response
  end

  def self.import_entities entities_name, rows
    if get_entities().include? entities_name
      update_entities(entities_name, rows)
    else
      create_entities(entities_name, rows)
    end
  end

  def self.create_entities entities_name, rows
    wit = Wit.new(access_token: Reports::Settings.wit_token)
    values = []
    rows.each do |r|
      if r[0] != nil
        values << {
                    "value" => "#{r[0]}",
                    "expressions" => 
                      [ r[0]]
                  }
      end
    end
    new_entity_obj = {
                        "doc" => "Value number of #{entities_name}",
                        "id" => entities_name,
                        "values" => values
                      }
    new_entity = wit.post_entities(new_entity_obj)
  end

  def self.update_entities entities_name, rows
    wit = Wit.new(access_token: Reports::Settings.wit_token)
    values = []
    rows.each do |r|
      if r[0] != nil
        values << {
                    "value" => "#{r[0]}",
                    "expressions" => 
                      [ r[0]]
                  }
      end
    end
    new_entity_obj = {
                        "doc" => "Value number of #{entities_name}",
                        "id" => entities_name,
                        "values" => values
                      }
    new_entity = wit.put_entities(entities_name, new_entity_obj)
  end

  def self.read_sheet spreadsheet, index
    sheet = spreadsheet.worksheet index.to_i
    return sheet.rows
  end

  def self.save_excel file_obj
    file_path = Rails.root.join('public', 'uploads', file_obj.original_filename)
    File.open( file_path, "wb") do |file|
      file.write(file_obj.read)
    end
    return file_path
  end

  def self.get_entities
    wit = Wit.new(access_token: Reports::Settings.wit_token)
    entities = wit.get_entities()
    return entities
  end

end
