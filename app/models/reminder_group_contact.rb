class ReminderGroupContact < ActiveRecord::Base
  belongs_to :reminder_group, class_name: "Ext::ReminderGroup"

  validates :address,:presence => true,
                 :numericality => true,
                 :length => { :minimum => 4, :maximum => 14 }
  validates_uniqueness_of :address, scope: :reminder_group_id

  attr_accessor :skip_callback

  before_save :encoding_address
  after_create :register_contact, if: -> { !skip_callback }

  def self.exist? reminder_group, address
    return false if reminder_group.nil? or !address.present?
    
    where(reminder_group_id: reminder_group.id, address: address).count > 0
  end

  def encoding_address
    self.address = self.address.force_encoding("utf-8")
  end

  def register_contact
    Contact.register [self.address], reminder_group.project
  end

  def upsert_contact(item, kind='csv')
    Contact.upsert(self, item, kind)
  end
end
