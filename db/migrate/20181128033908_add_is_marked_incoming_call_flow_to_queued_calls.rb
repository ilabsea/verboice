class AddIsMarkedIncomingCallFlowToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :is_marked_incoming_call_flow, :boolean, default: false
  end
end
