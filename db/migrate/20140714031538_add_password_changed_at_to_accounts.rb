class AddPasswordChangedAtToAccounts < ActiveRecord::Migration
  def change
    add_column :accounts, :password_changed_at, :datetime, default: nil

    add_index :accounts, :password_changed_at
  end

end
