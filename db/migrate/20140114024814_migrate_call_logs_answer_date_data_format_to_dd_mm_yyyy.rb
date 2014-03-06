class MigrateCallLogsAnswerDateDataFormatToDdMmYyyy < ActiveRecord::Migration
  def up
    call_log_answers = CallLogAnswer.where("value regexp '#{Date::PREVIOUS_DEFAULT_FORMAT.to_date_regex}'")
    call_log_answers.find_each do |call_log_answer|
      if call_log_answer.value.try(:old_date_format?)
        new_date_string = Ext::Parser::DateParser.parse(call_log_answer.value, Date::PREVIOUS_DEFAULT_FORMAT).to_string(Date::DEFAULT_FORMAT)
        call_log_answer.value = new_date_string
        call_log_answer.save
      end
    end
  end

  def down
    call_log_answers = CallLogAnswer.where("value regexp '#{Date::DEFAULT_FORMAT.to_date_regex}'")
    call_log_answers.find_each do |call_log_answer|
      if call_log_answer.value.try(:date_format?)
        new_date_string = Ext::Parser::DateParser.parse(call_log_answer.value, Date::DEFAULT_FORMAT).to_string(Date::PREVIOUS_DEFAULT_FORMAT)
        call_log_answer.value = new_date_string
        call_log_answer.save
      end
    end
  end
end
