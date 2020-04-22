module Ext
  class ReminderGroup < ExtActiveRecord
    belongs_to :project
    has_many :reminder_schedules, :dependent => :nullify

    assign_has_many_to "Project" ,:ext_reminder_groups, :class_name => "Ext::ReminderGroup", :dependent => :destroy
    assign_accepts_nested_attributes_for_to "Project", :ext_reminder_groups,
      :reject_if => lambda { |attributes| attributes[:name].blank?},
      :allow_destroy => true

    assign_attr_accessible_to "Project", :ext_reminder_group_attributes

    serialize :addresses, Array
    validates :name, :project, :presence => true
    validates :name, :uniqueness => { :scope => :project_id }

    attr_accessible :name, :addresses, :project_id
    attr_accessor :skip_callback

    before_save :encoding_addresses
    after_save :register_contacts, if: -> { !skip_callback }

    class << self
      def deserialized_to_array string
        array = []
        unless string.nil?
          ["---\n-", "-", "'", "\"", " "].each do |pattern|
            string.gsub!(pattern, "")
          end
          array = string.split("\n")
        end
        array
      end
    end

    def encoding_addresses
      addresses.map! {|x| x.force_encoding("utf-8")} if addresses
    end

    def register_contacts
      Contact.register addresses, project if has_addresses?
    end

    def has_addresses?
      not addresses.empty?
    end

    def register_address(address)
      #TODO refactoring
      if self.addresses.kind_of?(String)
        self.addresses = Ext::ReminderGroup.deserialized_to_array self.addersses
      end

      if self.addresses.include?(address)
        true
      else
        self.addresses.push(address)
        self.save
      end
    end

    def deregister_address(address)
      if addresses.include? (address)
        addresses.delete(address)
        save
      end
    end

    def import_contact_addresses(file)
      rows = CSV.parse(file.read, headers: true)
      rows.each do |row|
        phone_number = row['phone_number'].to_s.strip.to_number
        next unless phone_number.is_contact?
        addresses.push(phone_number) unless addresses.include? phone_number
      end

      Contact.register_from_file(rows, project) if save!(skip_callback: true)
    end
  end
end
