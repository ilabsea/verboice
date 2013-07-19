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

  def evaluate? conditions
    match = false
    conditions.each do |condition|
      if !condition.evaluate? persisted_variables
        match = false
        break
      else
        match = true
      end
    end if conditions
    match
  end

  def self.get address, project
    project.contacts.joins(:addresses).where(contact_addresses: {address: address}).first
  end

  def self.register addresses, project
    addresses.each do |address|
      contact = get address, project
      if contact.nil?
        contact = project.contacts.build
        contact.addresses.build(address: address)
        contact.save
      end
    end
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

  private

  def at_least_one_address
    errors[:base] << "You must provide at least one phone number" unless addresses.size > 0
  end
end
