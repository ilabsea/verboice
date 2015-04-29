class ConvertStepNameToUtf8OnTraces < ActiveRecord::Migration
  def up
    execute <<-SQL
      UPDATE traces SET step_name=CONVERT(CONVERT(CONVERT(step_name USING latin1) USING binary) USING utf8)
    SQL
  end

  def down
    execute <<-SQL
      UPDATE traces SET step_name=CONVERT(CONVERT(CONVERT(step_name USING utf8) USING binary) USING latin1)
    SQL
  end
end
