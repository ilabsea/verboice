module Ext
  class Condition
    attr_accessor :variable, :operator, :value, :data_type

    def initialize variable, operator, value, data_type
      @variable = variable
      @operator = operator
      @value = value
      @data_type = data_type
    end

    def self.build hash
      conditions = []
      hash.each do |k, v|
        conditions.push Ext::Condition.new v[:variable], v[:operator], v[:value], v[:data_type]
      end if hash
      conditions
    end

    #TODO refactoring to remove coupling
    def evaluate? project, persisted_variables
      match = false
      project_variable = project.project_variables.where(:name => self.variable).first

      if project_variable
        persisted_variables.each do |persisted_variable|
          if persisted_variable.project_variable_id == project_variable.id
            if data_type != "number"
              left_value = Date.today - eval("#{value}.#{data_type}")
              right_value = persisted_variable.value.try(:date_format?) ? Date.strptime(persisted_variable.value, Date::DEFAULT_FORMAT) : nil
            end

            match = Ext::Comparison.compare(left_value, operator, right_value) if left_value && right_value
            break if match
          end
        end
      else
        match = true # ignore is default if project variable doesn't exists
      end
      match
    end
  end
end