class AddAnsweredAtToQueuedCalls < ActiveRecord::Migration
  def change
    add_column :queued_calls, :answered_at, :datetime, :default => nil
  end
end
