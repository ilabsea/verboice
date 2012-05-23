require 'spec_helper'

module Commands
  describe PersistVariableCommand do
    it "should create a persisted variable storing a value with a given name" do
      contact = Contact.make
      project = Project.make account: contact.account
      call_log = CallLog.make project: project

      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.address

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should == :next

      PersistedVariable.all.size.should eq(1)
      PersistedVariable.first.value.should eq('2')
      PersistedVariable.first.name.should eq('foo')
      PersistedVariable.first.contact.should eq(contact)
    end

    it "should create a Contact if it doesn't exist" do
      call_log = CallLog.make
      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => '1234xxx'

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should == :next
      Contact.all.size.should eq(1)
      Contact.first.address.should eq('1234xxx')
      PersistedVariable.first.contact.should eq(Contact.first)
    end

    it "should replace the value of an existing variable" do
      contact = Contact.make
      project = Project.make account: contact.account
      call_log = CallLog.make project: project

      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.address

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should == :next

      PersistedVariable.all.size.should eq(1)
      PersistedVariable.first.value.should eq('2')
      PersistedVariable.first.name.should eq('foo')
      PersistedVariable.first.contact.should eq(contact)

      cmd = PersistVariableCommand.new 'foo', 1
      cmd.next = :next
      cmd.run(session).should == :next

      PersistedVariable.all.size.should eq(1)
      PersistedVariable.first.value.should eq('1')
      PersistedVariable.first.name.should eq('foo')
      PersistedVariable.first.contact.should eq(contact)
    end

    it "shouldn't set the variable if the caller address is unknown" do
      call_log = CallLog.make
      session = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => nil

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should == :next
      Contact.all.size.should eq(0)
      PersistedVariable.all.size.should eq(0)
      call_log.structured_details[0][:text].should == "Caller address is unknown. Variable 'foo' can't be saved for an anonymous contact."
    end
  end
end