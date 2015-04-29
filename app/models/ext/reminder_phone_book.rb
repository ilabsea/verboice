module Ext
	class ReminderPhoneBook < ExtActiveRecord
		belongs_to :project, :class_name => "::Project"
    has_one :patient

    belongs_to :type, :class_name => "ReminderPhoneBookType", :foreign_key => "type_id"

	assign_has_many_to "Project" ,:ext_reminder_phone_books, :class_name => "Ext::ReminderPhoneBook"

	validates :phone_number, :presence => true
	validates :phone_number, :uniqueness => { :scope => :type_id }

    before_destroy :destroy_patient

    private
    def destroy_patient
      self.patient.destroy if self.patient
    end
	end
end