class ReminderGroupContact < ActiveRecord::Base
  belongs_to :reminder_group, class_name: "Ext::ReminderGroup"

  validates_presence_of :reminder_group_id
  validates :address,:presence => true,
                 :numericality => true,
                 :length => { :minimum => 4, :maximum => 14 }
  validates_uniqueness_of :address, scope: :reminder_group_id

  def self.exist? reminder_group, address
    return false if reminder_group.nil? or !address.present?
    
    where(reminder_group_id: reminder_group.id, address: address).count > 0
  end

end
