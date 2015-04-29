class AddStepInteractionToCallLog < ActiveRecord::Migration
  def change
    add_column :call_logs, :step_interaction, :text
  end
end
