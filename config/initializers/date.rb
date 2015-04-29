class Date
  include DateComparison

  DEFAULT_ACTIVE_RECORD_FORMAT = "%Y-%m-%d"
  PREVIOUS_DEFAULT_FORMAT = "%Y-%m-%d"
  DEFAULT_FORMAT = "%d/%m/%Y"

  def to_string format = Date::DEFAULT_FORMAT
    self.strftime(format)
  end
end