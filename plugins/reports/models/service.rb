class Service

	def self.from_voice_to_speech audio_file, call_id
	  speech = Speech.new(Options.google_api_key)
    audio_url = "#{Options.verboice_url}/calls/#{call_id}/results/#{audio_file}"
    log = CallLog.find call_id
    file_audio = RecordingManager.for(log).result_path_for(audio_file)
    result = speech.recognize file_audio, Options.language_recognition
    return result['transcript']
	end

	def self.from_speech_to_understanding message
    wit = Wit.new(access_token: Options.wit_token)
    json_response = wit.message message
    return json_response
	end

	def self.extract_understanding cases
		properties = {}
		index = 0
    cases.each do |obj|
    	properties[index] = {}
    	properties[index]["number"] = obj["entities"]["number"][0]["value"]
    	properties[index]["symptoms"] = obj["entities"]["symptom"].map {|s| s["value"] }
    end
	  return properties
	end

  def self.import_entities entities_name, rows
    wit = Wit.new(access_token: Options.wit_token)
    values = []
    rows.each do |r|
      values << {
                  "value" => "#{r[0]}",
                  "expressions" => 
                    [ r[0]]
                }
    end
    new_entity_obj = {
                        "doc" => "Value number of #{entities_name}",
                        "id" => entities_name,
                        "values" => values
                      }
    new_entity = wit.post_entities(new_entity_obj)
  end

  def self.read_sheet spreadsheet, index
    sheet = spreadsheet.worksheet index.to_i
    return sheet.rows
  end

  def self.save_csv file_obj
    file_path = Rails.root.join('public', 'uploads', file_obj.original_filename)
    File.open( file_path, 'wb') do |file|
      file.write(file_obj.read)
    end
    return file_path
  end

end