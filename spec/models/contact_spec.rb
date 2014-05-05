require 'spec_helper'

describe Contact do
  
  let(:project) { Project.make }
  let(:contact) { Contact.make project: project }
  let(:project_var1) { project.project_variables.make :name => "var1" }
  let(:project_var2) { project.project_variables.make :name => "var2" }

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

end
