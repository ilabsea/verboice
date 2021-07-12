module Ext
  class ReminderGroup < ExtActiveRecord
    belongs_to :project
    has_many :reminder_schedules, :dependent => :nullify
    has_many :reminder_group_contacts, foreign_key: :reminder_group_id, :dependent => :destroy

    assign_has_many_to "Project" ,:ext_reminder_groups, :class_name => "Ext::ReminderGroup", :dependent => :destroy
    assign_accepts_nested_attributes_for_to "Project", :ext_reminder_groups,
      :reject_if => lambda { |attributes| attributes[:name].blank?},
      :allow_destroy => true

    accepts_nested_attributes_for :reminder_group_contacts, allow_destroy: true

    assign_attr_accessible_to "Project", :ext_reminder_group_attributes

    serialize :addresses, Array
    serialize :sync_config, Hash
    validates :name, :project, :presence => true
    validates :name, :uniqueness => { :scope => :project_id }

    after_save :touch_sync_status_updated_at, :if => :enable_sync_changed?

    attr_accessible :name, :addresses, :project_id, :mode, :enable_sync, :sync_config, :reminder_group_contacts_attributes

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

    def has_addresses?
      !!reminder_group_contacts.length || (not addresses.empty?)
    end

    def register_address(address)
      self.reminder_group_contacts.find_or_create_by_address(address)
    end

    def deregister_address(address)
      contact = self.reminder_group_contacts.find_by_address(address)
      contact.destroy if contact.present?
    end

    def import_contact_addresses(file)
      rows = CSV.parse(file.read, headers: true)
      rows.each do |row|
        phone_number = row['phone_number'].to_s.strip.to_number

        upsert_reminder_group_contact(phone_number, row, 'csv')
      end

    end

    def upsert_reminder_group_contacts(collection)
      collection.each do |item|
        # addresses_0_phoneNumber
        fields = self.sync_config[:phone_number_field].split("_").map { |s| s.number? ? s.to_i : s }
        phone_number = fields.inject(item) { |obj, field| obj.try(:[], field) }.to_s.strip.to_number

        upsert_reminder_group_contact(phone_number, item, 'json')
      end
      self.touch
    end

    def upsert_reminder_group_contact(phone_number, item, kind='csv')
      return unless phone_number.is_contact?

      reminder_group_contact = self.reminder_group_contacts.find_or_initialize_by_address(phone_number)
      reminder_group_contact.upsert_contact(item, kind) if reminder_group_contact.save(skip_callback: true)
    end

    private
      def touch_sync_status_updated_at
        self.touch(:sync_status_updated_at)
      end
  end
end
