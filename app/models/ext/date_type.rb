class DateType
  TYPE_OPTIONS = {:week => 1, :month => 2}

  def initialize type
    @type = type
  end

  def days
    @value = 7 if @type == TYPE_OPTIONS[:week]
    @value = 30 if @type == TYPE_OPTIONS[:month]
    @value
  end
end