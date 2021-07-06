module Concerns::Contacts::SyncWithGoData
  extend ActiveSupport::Concern

  included do
    class << self
      def upsert(reminder_group_contact, row, kind='csv')
        @variables = kind == 'csv' ? row.headers : reminder_group_contact.reminder_group.sync_config[:variables]
        @variables ||= []
        @kind = kind

        upsert_contact(reminder_group_contact, row)
      end

      private
        def upsert_contact(reminder_group_contact, row)
          @project = reminder_group_contact.reminder_group.project
          contact = assign_contact(reminder_group_contact.address)
          contact.persisted_variables_attributes = build_persisted_variables_attrs(contact, row)
          contact.save
        end

        def assign_contact(address)
          if contact = get(address, @project).presence
            contact = self.find(contact.id)
          else
            contact = @project.contacts.build
            contact.addresses.build(address: address)
          end

          contact
        end

        def implicit_keys
          @implicit_keys ||= ImplicitVariable.subclasses.collect(&:key)
        end

        def build_persisted_variables_attrs(contact, row)
          attrs = []

          @variables.each do |var|
            key = @kind == 'csv' ? var : var[:destination]
            value = @kind == 'csv' ? row[key] : row[var[:source]]
            project_var = @project.project_variables.select { |v| v.name == key }.first

            next if value.blank? || (implicit_keys.exclude?(key) && project_var.nil?)

            attrs.push(build_attr(key, value, contact, project_var))
          end

          attrs
        end

        def build_attr(key, value, contact, project_var)
          _attr = { value: value }

          if implicit_keys.include? key
            _attr[:implicit_key] = key
            _attr[:id] = contact.persisted_variables.select{ |v| v.implicit_key == key }.first.try(:id)
          else
            _attr[:project_variable_id] = project_var.id
            _attr[:id] = contact.persisted_variables.select{ |v| v.project_variable_id == project_var.id }.first.try(:id)
          end

          _attr
        end
    end
  end
end
