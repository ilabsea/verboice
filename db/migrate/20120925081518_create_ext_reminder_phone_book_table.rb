class CreateExtReminderPhoneBookTable < ActiveRecord::Migration
  def change
  	create_table :ext_reminder_phone_books do |t|
  		t.references :project
  		t.string :name
  		t.string :phone_number
  		t.timestamp
  	end	
  end
end
