require 'spec_helper'

describe Ext::ReminderPhoneBookType do
  before(:each) do
    project = Project.make
    @valid = {
      :name => "Reminder Type",
      :project_id => project.id
    }
  end

  it "should create a new reminder phone book with valid attributes" do
    type = Ext::ReminderPhoneBookType.new @valid
    type.save.should eq true
  end

  it "should require name" do
    attrs = @valid.merge(:name => "")
    type = Ext::ReminderPhoneBookType.new attrs
    type.save.should eq false
  end

  it "should require type" do
    attrs = @valid.merge(:project_id => nil)
    type = Ext::ReminderPhoneBookType.new attrs
    type.save.should eq false
  end

  it "should require name to be unique in scope of project" do
    type = Ext::ReminderPhoneBookType.new @valid
    type.save.should eq true
    type = Ext::ReminderPhoneBookType.new @valid
    type.save.should eq false
  end

end
