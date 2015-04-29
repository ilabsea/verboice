class CreateExtReminderGroups < ActiveRecord::Migration
  def change
    create_table :ext_reminder_groups do |t|
      t.string :name
      t.string :description
      t.references :project
      t.string :addresses

      t.timestamps
    end
    add_index :ext_reminder_groups, :project_id
  end
end
