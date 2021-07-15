require 'spec_helper'

describe ReminderGroupContact do
  let!(:project) { Project.make }
  let!(:reminder_group) { Ext::ReminderGroup.create(name: 'reminder_group', project_id: project.id) }

  describe 'before_save :encoding_address' do
    context 'Encoding::ASCII_8BIT' do
      let(:reminder_group_contact) { reminder_group.reminder_group_contacts.create(address: "011".encode(Encoding::ASCII_8BIT)) }

      it 'should encode addresses to utf-8' do
        reminder_group_contact.address.encoding.should eq(Encoding::UTF_8)
      end
    end
  end

  describe 'after_create register_contact' do
    let!(:reminder_group_contact) { reminder_group.reminder_group_contacts.create(address: "011222333") }

    context 'new address' do
      it 'register new contact' do
        project.contacts.length == 1
        project.contacts.first.addresses.first.address.should eq("011222333")
      end
    end

    context 'existed address' do
      before {
        reminder_group.reminder_group_contacts.create(address: "011222333")
      }

      it 'does not create another contact' do
        project.contacts.length == 1
        project.contacts.first.addresses.first.address.should eq("011222333")
      end
    end
  end
end
