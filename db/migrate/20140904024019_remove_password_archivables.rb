class RemovePasswordArchivables < ActiveRecord::Migration
  def up
    drop_table :old_passwords
  end

  def down
    create_table :old_passwords do |t|
      t.string :encrypted_password, :null => false
      t.string :password_salt
      t.string :password_archivable_type, :null => false
      t.integer :password_archivable_id, :null => false
      t.timestamps
    end
    add_index :old_passwords, [:password_archivable_type, :password_archivable_id], :name => :index_password_archivable
  end
end
