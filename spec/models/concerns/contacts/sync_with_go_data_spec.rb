require 'spec_helper'

describe Concerns::Contacts::SyncWithGoData do
  describe '.upsert' do
    let!(:project) { Project.make }
    let!(:project_var) { project.project_variables.make :name => "last_contact_date" }
    let!(:project_var1) { project.project_variables.make :name => "var1" }
    let!(:reminder_group) { Ext::ReminderGroup.create(:name => 'Reminder Group', :project_id => project.id, :sync_config => {:phone_number_field => 'Addresses_0_Phone number', :variables => [{:source => 'Date of last contact', :destination => 'last_contact_date'}]}) }
    let!(:reminder_group_contact) { reminder_group.reminder_group_contacts.create(address: '011222333')}
    let!(:json_item) {
      {
        "Date of last contact" => "2021-06-28T00:00:00.000Z",
        "Addresses" => [ { "Phone number" => "011222333" } ]
      }
    }

    let!(:csv_item) {
      file = File.open(File.join(Rails.root, '/spec/fixtures/reminder_group_addresses.csv'))
      rows = CSV.parse(file.read, headers: true)
      rows[0]
    }

    context 'json' do
      before {
        Contact.upsert(reminder_group_contact, json_item, 'json')
      }

      it 'add new contact' do
        Contact.count.should eq(1)
      end

      it 'add a new contact address' do
        Contact.first.addresses.first.address.should eq("011222333")
      end

      it 'add persisted_variables as last_contact_date' do
        Contact.first.persisted_variables.length.should eq(1)
      end
    end

    context 'csv' do
      before {
        Contact.upsert(reminder_group_contact, csv_item, 'csv')
      }

      it 'add new contact' do
        Contact.count.should eq(1)
      end

      it 'add a new contact address' do
        Contact.first.addresses.first.address.should eq("011222333")
      end

      it 'add persisted_variables as implicit_key' do
        Contact.first.persisted_variables.first.value.should eq("km")
        Contact.first.persisted_variables.first.implicit_key.should eq('language')
      end

      it 'add persisted_variables as v1' do
        Contact.first.persisted_variables.last.value.should eq("1")
        Contact.first.persisted_variables.last.project_variable_id.should eq(project_var1.id)
      end
    end
  end
end
