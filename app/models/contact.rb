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

class Contact < ActiveRecord::Base
  belongs_to :project
  has_many :addresses, :dependent => :destroy, :class_name => 'ContactAddress', :inverse_of => :contact
  has_many :persisted_variables, :dependent => :destroy, :inverse_of => :contact
  has_many :recorded_audios, :dependent => :destroy
  has_many :project_variables, :through => :project

  accepts_nested_attributes_for :persisted_variables,
    :reject_if => lambda { |attributes| attributes[:value].blank? || (attributes[:project_variable_id].blank? && attributes[:implicit_key].blank?) },
    :allow_destroy => true
  accepts_nested_attributes_for :addresses, :allow_destroy => true

  attr_accessible :addresses_attributes, :anonymous, :persisted_variables_attributes

  validates_presence_of :project
  validate :at_least_one_address

  class << self
    def get address, project
      project.contacts.joins(:addresses).where(contact_addresses: {address: address}).first
    end

    def register addresses, project
      addresses.each do |address|
        contact = get address, project
        if contact.nil?
          contact = project.contacts.build
          contact.addresses.build(address: address)
          contact.save
        end
      end
    end

    def remove_duplicate_address_and_keep_last_update project, project_variable
      removing_contact_ids = []
      removing_contact_address_ids = []

      project.contacts.each do |contact|
        next if  removing_contact_ids.include?(contact.id)
        current_latest_contact = contact

        project.contacts.each do |cursor_contact|
          next if cursor_contact.id <= current_latest_contact.id

          next if  removing_contact_ids.include?(cursor_contact.id)

          cursor_contact.addresses.each do |cursor_contact_address|
            next if removing_contact_address_ids.include?(cursor_contact_address.id)

            if current_latest_contact.has_address?(cursor_contact_address.address)

              if current_latest_contact.last_updated_value_than?(cursor_contact, project_variable)
                if cursor_contact.has_only_one_address?
                  removing_contact_ids.push(cursor_contact.id) unless removing_contact_ids.include?(cursor_contact.id)
                else
                  removing_contact_address_ids.push(cursor_contact_address.id) unless removing_contact_address_ids.include?(cursor_contact_address.id)
                end
              else
                if current_latest_contact.has_only_one_address?
                  removing_contact_ids.push(current_latest_contact.id) unless removing_contact_ids.include?(current_latest_contact.id)
                else
                  current_latest_contact_address = current_latest_contact.get_address(cursor_contact_address.address)
                  if current_latest_contact_address && !removing_contact_address_ids.include?(current_latest_contact_address.id)
                    removing_contact_address_ids.push(current_latest_contact_address.id)
                  end
                end
                current_latest_contact = cursor_contact
              end
            end
          end

        end
      end

      ContactAddress.remove removing_contact_address_ids, project

      remove removing_contact_ids, project
    end

    def remove ids, project
      project.contacts.each do |contact|
        contact.destroy if ids.include?(contact.id)
      end
    end
  end

  def evaluate? condition
    (condition && condition.evaluate?(project, persisted_variables)) ? true : false
  end

  def register address
     self.addresses.where(address: address).first_or_create!
  end

  def remove_address address
    addresses.each do |x|
      x.destroy if x.address == address
    end
  end

  def as_json(options={})
    super(options.merge({except: [:anonymous]}))
  end

  def first_address
    addresses.first.try(&:address)
  end

  def next_address(address)
    addresses = self.addresses.order(:id).map(&:address)
    addresses.drop_while { |addr| addr != address }.second 
  end

  def has_address? address
    addresses.each do |contact_address|
      return true if Tel.without_prefix(contact_address.address) == Tel.without_prefix(address)
    end

    false
  end

  def has_only_one_address?
    addresses.count == 1
  end

  def last_updated_value_than?(right_contact, project_variable)
    result = false

    left_persisted_var = persisted_variables.where(project_variable_id: project_variable.id).order('updated_at desc').first
    right_persisted_var = right_contact.persisted_variables.where(project_variable_id: project_variable.id).order('updated_at desc').first

    if right_persisted_var.nil?
      result = true
    elsif left_persisted_var.nil?
      result = false
    else
      result = (left_persisted_var.updated_at > right_persisted_var.updated_at ? true : false)
    end

    result
  end

  def get_address(address)
    addresses.each do |contact_address|
      return contact_address if Tel.without_prefix(contact_address.address) == Tel.without_prefix(address)
    end

    nil
  end

  private

  def at_least_one_address
    errors[:base] << "You must provide at least one phone number" unless addresses.size > 0
  end

end
