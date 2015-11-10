# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

namespace :contact_address do
  desc "Migrate call log trace to step interaction"
  task :remove_duplicate => :environment do
    log("Removing duplication address in contact address\n") do
      duplicates = ContactAddress.select("project_id, address, count(*)").group("project_id, address").having("count(*) > 1").to_a
      duplicates.each_with_index do |duplicate, i|
        contact_addresses = ContactAddress.where(project_id: duplicate.project_id, address: duplicate.address)
        deleted = false
        contact_addresses.each do |contact_address|
          if PersistedVariable.where(contact_id: contact_address.contact_id).count == 0
            contact_address.contact.destroy
            deleted = true
          end
        end

        unless deleted
          contact_addresses.map(&:contact).sort_by do |contact|
            contact.persisted_variables.order(:updated_at).last.updated_at
          end[0..-2].each do |contact|
            contact.destroy
          end
        end
        print "Processing #{i + 1}/#{duplicates.count}\r"
      end

      puts
    end
  end

  desc "Replace country code with zero"
  task :replace_country_code_with_zero, [:project_id, :country_code] => :environment do |t, args|
    log("Replacing country code to zero in contact address\n") do
      project_id = args[:project_id]
      country_code = args[:country_code]

      contact_addresses = ContactAddress.where(project_id: project_id)
      
      ContactAddress.transaction do
        contact_addresses.each_with_index do |contact_address, i|
          print "Processing #{i + 1}/#{contact_addresses.count}\r"
          if contact_address.address.start_with?(country_code)
            address_with_zero_prefix = contact_address.address.gsub(/^#{country_code}/, "0")
            
            existing_contact_addresses = ContactAddress.where(project_id: project_id, address: address_with_zero_prefix)
            if existing_contact_addresses.count > 0
              if PersistedVariable.where(contact_id: contact_address.contact_id).count == 0
                contact_address.contact.destroy
                next
              else
                if PersistedVariable.where(contact_id: existing_contact_addresses.first.contact.id).count == 0
                  existing_contact_addresses.first.contact.destroy
                end
              end
            end

            contact_address.address = address_with_zero_prefix
            contact_address.save!
          end
        end
      end

    end
  end
end
