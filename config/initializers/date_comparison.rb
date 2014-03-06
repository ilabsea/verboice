module DateComparison
  def equal? time
    self == time
  end

  def greater_than? time
    self > time
  end

  def less_than? time
    self < time
  end

  def greater_or_equal? time
    greater_than? time or equal? time
  end

  def less_or_equal? time
    less_than? time or equal? time
  end
end