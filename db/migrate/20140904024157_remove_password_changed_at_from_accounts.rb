class RemovePasswordChangedAtFromAccounts < ActiveRecord::Migration
  def up
    remove_column :accounts, :password_changed_at
  end

  def down
    add_column :accounts, :password_changed_at, :datetime, default: nil

    add_index :accounts, :password_changed_at
  end
end
