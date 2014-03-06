class AddLocaleToAccounts < ActiveRecord::Migration
  def change
    add_column :accounts, :locale, :string
  end
end
