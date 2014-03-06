module Ext
  class Comparison
    def self.compare left, operator, right
      operator = "==" if operator == "="
      eval "left#{operator}right"
    end
  end
end