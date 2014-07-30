class AddRoleToAccounts < ActiveRecord::Migration
  def change
    add_column :accounts, :role, :integer, default: Account::USER
  end
end
