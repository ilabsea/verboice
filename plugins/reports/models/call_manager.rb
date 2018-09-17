class CallManager
  def self.handle_call_finished call_id
    audio_file = Reports::Settings.verboice_first_audio_file

    message = Service::from_voice_to_speech(audio_file, call_id)
    # message = "There are two cases of headaches and eye problems and one case of wheezing in Hanoi"
    
    json_response = Service::from_speech_to_understanding message

    yield json_response if json_response
  end
end
