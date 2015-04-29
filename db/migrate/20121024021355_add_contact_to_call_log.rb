class AddContactToCallLog < ActiveRecord::Migration
  def change
    add_column :call_logs, :contact_id, :integer, :defaul => nil
  end
end
