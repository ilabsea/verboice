module Ext
  class ReminderGroup < ExtActiveRecord
    belongs_to :project
    has_many :reminder_schedules, :dependent => :nullify
    has_many :reminder_group_contacts, foreign_key: :reminder_group_id

    assign_has_many_to "Project" ,:ext_reminder_groups, :class_name => "Ext::ReminderGroup", :dependent => :destroy
    assign_accepts_nested_attributes_for_to "Project", :ext_reminder_groups,
      :reject_if => lambda { |attributes| attributes[:name].blank?},
      :allow_destroy => true

    assign_attr_accessible_to "Project", :ext_reminder_group_attributes

    serialize :addresses, Array
    serialize :sync_config, Hash
    validates :name, :project, :presence => true
    validates :name, :uniqueness => { :scope => :project_id }

    attr_accessible :name, :addresses, :project_id, :mode, :enable_sync, :sync_config
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
      # Contact.register addresses, project if has_addresses?
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
      ReminderGroupContact.transaction do
        rows.each do |row|
          phone_number = row['phone_number'].to_s.strip.to_number
          # TODO Refactor to reminder_group_contacts
          # addresses.push(phone_number) unless addresses.include? phone_number

          upsert_reminder_group_contact(phone_number, row, 'csv')
        end
      end

    end

    def upsert_reminder_group_contacts(collection)
      collection.each do |item|
        # addresses_0_phoneNumber
        fields = self.sync_config[:phone_number_field].split("_").map { |s| s.number? ? s.to_i : s }
        phone_number = fields.inject(item) { |obj, field| obj.try(:[], field) }.to_s.strip.to_number

        upsert_reminder_group_contact(phone_number, item, 'json')
      end
    end

    def upsert_reminder_group_contact(phone_number, item, kind='csv')
      return unless phone_number.is_contact?

      reminder_group_contact = self.reminder_group_contacts.find_or_create_by_address(phone_number)
      reminder_group_contact.upsert_contact(item, kind)
    end
  end
end
