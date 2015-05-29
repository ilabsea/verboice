class AddContactToCallLog < ActiveRecord::Migration
  def change
    # CONFLICT: Already created on previous version
    # add_column :call_logs, :contact_id, :integer, :defaul => nil
  end
end
