class AddAuthTokenToAccounts < ActiveRecord::Migration
  def change
    add_column :accounts, :auth_token, :string
  end
end
