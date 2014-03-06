class String
  def number?
    self == self.to_i.to_s
  end

  #TODO: be careful modify/delete
  def old_date_format?
    begin
      Date.strptime(self, Date::PREVIOUS_DEFAULT_FORMAT)
    rescue
      nil
    end
  end

  def date_format?
    begin
      Date.strptime(self, Date::DEFAULT_FORMAT)
    rescue
      nil
    end
  end

  def date_time_format?
    begin
      DateTime.strptime(self, DateTime::DEFAULT_FORMAT)
    rescue
      nil
    end
  end

  def audio_mime_type?
    ["audio/mp3", "audio/mpeg", "audio/x-wav", "audio/wav"].include? self
  end

  def mpeg_mime_type?
    ["audio/mp3", "audio/mpeg"].include? self
  end

  def to_date_regex()
    "^" + self.gsub("%Y", "[0-9]{4}").gsub("%m", "[0-9]{2}").gsub("%d", "[0-9]{2}") + "$"
  end

end
