class AddStateToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :state, :string, default: "queued"
  end
end
