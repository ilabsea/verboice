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

require 'spec_helper'

describe Contact do

  let!(:project) do 
    account = Account.make
    account.projects.make
  end
  
  let(:contact) { Contact.make project: project }
  let(:project_var1) { project.project_variables.make :name => "var1" }
  let(:project_var2) { project.project_variables.make :name => "var2" }

  it '#semicolon_separated_addresses' do
    contact = Contact.make :project => project, :addresses_attributes => [{:address => '123'}, {:address => '456'}]
    contact.semicolon_separated_addresses.should eq("123;456")
  end
  describe ".register" do
    before(:each) do
      @project = Project.make
      @addresses = ["1000", "1001"]
    end

    it "should contacts is empty" do
      @project.contacts.size.should == 0
    end

    it "should create non existing addresses" do
      Contact.register @addresses, @project

      @project.contacts.size.should == 2
      @project.contacts[0].first_address.should == "1000"
      @project.contacts[1].first_address.should == "1001"
    end

    it "should ignore existing addresses" do
      @contact = Contact.make project: @project
      @contact.addresses.destroy_all # reset the default one when contact has been created
      @contact.addresses.make address: "1000"

      Contact.register @addresses, @project

      @project.contacts.reload
      @project.contacts.size.should == 2
      @project.contacts[0].first_address.should == "1000"
      @project.contacts[1].first_address.should == "1001"
    end
  end

  describe "#register" do
    before(:each) do
      @project = Project.make
      @contact = Contact.make project: @project
      @contact.addresses.destroy_all
    end

    it "should addresses is empty" do
      @contact.addresses.size.should == 0
    end

    it "should create non existing address" do
      @contact.register "1000"

      @contact.reload.addresses.size.should == 1
    end

    it "should ignore existing address" do
      @contact.addresses.make address: "1000"
      @contact.register "1000"

      @contact.reload.addresses.size.should == 1
    end
  end

  describe ".get" do
    before(:each) do
      @project = Project.make
      @contact = Contact.make project: @project
      @contact.addresses.make address: "1000"
    end

    it "should return the contact when it has address 1000" do
      Contact.get(1000, @project).should_not be_nil
    end

    it "should return nil when it doesn't have 9999" do
      Contact.get(9999, @project).should be_nil
    end
  end

  describe "#last_updated_value_than?" do
    context "return true when the right variable value is nil" do
      before(:each) do
        @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000"), ContactAddress.create(project: project, address: "2000")]
        @contact2 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "3000"), ContactAddress.create(project: project, address: "8551000")]

        PersistedVariable.make contact: @contact1, project: project, project_variable: project_var1, updated_at: Time.now
      end

      it { expect(@contact1.last_updated_value_than?(@contact2, project_var1)).to eq(true) }
    end

    context "return false when the left variable value is nil" do
      before(:each) do
        @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000"), ContactAddress.create(project: project, address: "2000")]
        @contact2 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "3000"), ContactAddress.create(project: project, address: "8551000")]

        PersistedVariable.make contact: @contact2, project: project, project_variable: project_var1, updated_at: Time.now
      end

      it { expect(@contact1.last_updated_value_than?(@contact2, project_var1)).to eq(false) }
    end

    context "return true when the left variable value is last modified" do
      before(:each) do
        @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000"), ContactAddress.create(project: project, address: "2000")]
        @contact2 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "3000"), ContactAddress.create(project: project, address: "8551000")]

        PersistedVariable.make contact: @contact1, project: project, project_variable: project_var1, updated_at: Time.now + 1.days
        PersistedVariable.make contact: @contact2, project: project, project_variable: project_var1, updated_at: Time.now
      end

      it { expect(@contact1.last_updated_value_than?(@contact2, project_var1)).to eq(true) }
    end

    context "return false when the right variable value is last modified" do
      before(:each) do
        @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000"), ContactAddress.create(project: project, address: "2000")]
        @contact2 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "3000"), ContactAddress.create(project: project, address: "8551000")]

        PersistedVariable.make contact: @contact1, project: project, project_variable: project_var1, updated_at: Time.now
        PersistedVariable.make contact: @contact2, project: project, project_variable: project_var1, updated_at: Time.now + 1.days
      end

      it { expect(@contact1.last_updated_value_than?(@contact2, project_var1)).to eq(false) }
    end
    
  end

  describe ".remove_duplicate_address_and_keep_last_update" do
    context "keep the first one when it's no varaible value" do
      before(:each) do
        @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "01000")]
        @contact11 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000")]
      end

      it { expect(Contact.count).to eq(2)}

      it "should remove duplicated contacts when it has only one address" do
        Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

        expect(Contact.count).to eq(1)
      end

      it "should keep the first one" do
        Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

        expect(Contact.last.id).to eq(@contact1.id)
      end
    end

    context "remove only address which is duplicated when it has many addresses" do
      before(:each) do
        @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "01000")]
        @contact11 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000")]
        @contact3 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "8551000"), ContactAddress.create(project: project, address: "3000")]
      end

      it { expect(Contact.count).to eq(3) }

      it "should remove contact11" do
        Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

        expect(Contact.count).to eq(2)
      end

      it "should keep the first one" do
        Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

        expect(Contact.first.id).to eq(@contact1.id)
      end

      it "should remove contact address of the contact3" do
        Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

        expect(Contact.last.id).to eq(@contact3.id)
      end
    end

    context "keep the existing variable value" do
      context "duplicate primary address" do
        before(:each) do
          @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "01000")]
          @contact11 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000")]

          PersistedVariable.make contact: @contact11, project: project, project_variable: project_var1, updated_at: Time.now
        end

        it "should the last when it has variable value" do
          Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

          expect(Contact.last.id).to eq(@contact11.id)
        end
      end

      context "duplicate secondary address" do
        it "should remove contact address of the first contact" do
          @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "3000"), ContactAddress.create(project: project, address: "8551000")]
          @contact11 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000")]

          PersistedVariable.make contact: @contact11, project: project, project_variable: project_var1, updated_at: Time.now

          Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

          expect(Contact.first.id).to eq(@contact1.id)
          expect(@contact1.reload.addresses.count).to eq(1)
          expect(Contact.last.id).to eq(@contact11.id)
        end

        it "should remove the first contact when it has variable value" do
          @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "01000")]
          @contact11 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "3000"), ContactAddress.create(project: project, address: "1000")]

          PersistedVariable.make contact: @contact11, project: project, project_variable: project_var1, updated_at: Time.now

          Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

          expect(Contact.count).to eq(1)
          expect(Contact.last.id).to eq(@contact11.id)  
          expect(@contact11.reload.addresses.count).to eq(2)
        end
      end
    end

    context "keep the last modified" do
      context "only one primary address" do
        before(:each) do
          @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "01000")]
          @contact2 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000")]

          PersistedVariable.make contact: @contact1, project: project, project_variable: project_var1, updated_at: Time.now
          PersistedVariable.make contact: @contact2, project: project, project_variable: project_var1, updated_at: Time.now + 1.days
        end

        it { expect(Contact.count).to eq(2)}

        it "should remove contact that is not last modified" do
          Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

          expect(Contact.count).to eq(1)
          expect(Contact.last.id).to eq(@contact2.id)
        end
      end

      context "has many addresses" do
        before(:each) do
          @contact1 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "1000"), ContactAddress.create(project: project, address: "2000")]
          @contact2 = Contact.make project: project, addresses: [ContactAddress.create(project: project, address: "3000"), ContactAddress.create(project: project, address: "8551000")]

          PersistedVariable.make contact: @contact1, project: project, project_variable: project_var1, updated_at: Time.now
          PersistedVariable.make contact: @contact2, project: project, project_variable: project_var1, updated_at: Time.now + 1.days
        end

        it { expect(Contact.count).to eq(2)}

        it "should remove address that has duplicated" do
          Contact.remove_duplicate_address_and_keep_last_update(project, project_var1)

          expect(Contact.count).to eq(2)

          expect(Contact.first.id).to eq(@contact1.id)
          expect(@contact1.addresses.count).to eq(1)
          expect(Contact.last.id).to eq(@contact2.id)
          expect(@contact2.addresses.count).to eq(2)
        end
      end
    end
  end

end