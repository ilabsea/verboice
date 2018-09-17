class Speech
  attr_reader :api_key

  def initialize api_key
    @api_key = api_key
  end

  def recognize audio_file, language = 'en-US', encoding = 'linear16', bit_rate = 8000, max_alternatives = 3
    url = "https://speech.googleapis.com/v1/speech:recognize?key=#{@api_key}"
    content = fetch_content audio_file
    
    params = {
      config: {
        encoding: encoding,
        sampleRateHertz: bit_rate,
        languageCode: language,
        maxAlternatives: max_alternatives
      },
      audio: {
        content: content
      }
    }

    response = do_post_request url, params
    if response.code == 200
      transcript response.body
    end
  end

  def transcript response
    results = JSON.parse(response)

    if results["results"] && results["results"].size > 0
      result = {'confidence' => 0, 'transcript' => ''}
      results["results"].first["alternatives"].each do |r|
        result = r if result['confidence'] < r['confidence']
      end

      Rails.logger.info "Result - transcript: #{result['transcript']}, confidence: #{result['confidence']}"

      result
    end
  end

  def fetch_content file
    content = nil

    begin
      content = File.open(file, 'rb') { |f| Base64.strict_encode64(f.read) }
    rescue Exception => e
      Rails.logger.info "File #{file} doesn't exist"
    end

    content
  end

  def do_post_request url, params
    RestClient.post(url, params.to_json, {content_type: :json, accept: :json})
  end

end
