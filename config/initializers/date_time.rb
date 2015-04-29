class DateTime
  include DateComparison
  
  DEFAULT_FORMAT  = '%d/%m/%Y %H:%M %z'
  DEFAULT_FORMAT_WITHOUT_TIMEZONE = '%d/%m/%Y %H:%M'

  def to_string format = DateTime::DEFAULT_FORMAT, time_zone = "UTC"
    zone = self.in_time_zone(time_zone)
    zone.strftime(format)
  end
end