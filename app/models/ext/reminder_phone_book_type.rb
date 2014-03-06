module Ext
  class ReminderPhoneBookType < ExtActiveRecord
    belongs_to :project
    has_many :reminder_phone_books, :class_name => "ReminderPhoneBook", :foreign_key => "type_id", :dependent => :nullify
    attr_accessible :description, :name, :project_id

    assign_has_many_to "Project" ,:ext_reminder_phone_book_types, :class_name => "Ext::ReminderPhoneBookType", :dependent => :destroy
    assign_accepts_nested_attributes_for_to "Project", :ext_reminder_phone_book_types, 
      :reject_if => lambda { |attributes| attributes[:name].blank?},
      :allow_destroy => true

    assign_attr_accessible_to "Project", :ext_reminder_phone_book_types_attributes

    validates :name, :project, :presence => true
    validates :name, :uniqueness => { :scope => :project_id }
  end
end
