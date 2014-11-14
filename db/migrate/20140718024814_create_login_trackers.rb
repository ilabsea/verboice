class CreateLoginTrackers < ActiveRecord::Migration
  def change
    create_table :login_trackers do |t|
      t.string :email
      t.string :origin_ip
      t.string :status
      t.string :marked_as
      t.datetime :logged_in_at

      t.timestamps
    end

    add_index :login_trackers, :origin_ip
  end
end
