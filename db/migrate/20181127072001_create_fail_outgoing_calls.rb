class CreateFailOutgoingCalls < ActiveRecord::Migration
  def change
    create_table :fail_outgoing_calls do |t|
      t.string  :address
      t.integer :call_flow_id

      t.timestamps
    end

    add_index :fail_outgoing_calls, :address, :unique => true
  end
end
