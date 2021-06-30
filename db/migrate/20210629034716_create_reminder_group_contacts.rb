class CreateReminderGroupContacts < ActiveRecord::Migration
  def change
    create_table :reminder_group_contacts do |t|
      t.belongs_to :reminder_group
      t.string :address, :limit => 10 # format must be started with zero(0)


      t.timestamps
    end

    add_index :reminder_group_contacts, [:reminder_group_id, :address], using: :btree, unique: true
  end
end
