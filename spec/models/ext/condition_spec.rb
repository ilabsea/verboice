require 'spec_helper'

describe Ext::Condition do
  it "should return an empty array when hash is undefined" do
    hash = nil
    array = Ext::Condition.build hash
    array.size.should eq 0
  end

  it "should build array from hash values" do
    hash = {"0" => {variable: 'testing', operator: '=', value: '5', data_type: 'number'} }
    array = Ext::Condition.build hash
    array.first.class.should eq Ext::Condition
    array.first.variable.should eq 'testing'
    array.first.operator.should eq '='
    array.first.data_type.should eq 'number'
    array.first.value.should eq '5'
  end

  describe "#evaluate?" do
    before(:each) do
      @contact = Contact.make
      @project = Project.make
      @project_variable = ProjectVariable.make :name => "var1", project_id: @project.id
    end

    it "should return true when project variable doesn't exists" do
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "5")
      persisted_variables = @contact.persisted_variables
      condition = Ext::Condition.new "var2", "=", "5", 'number'

      condition.evaluate?(@project, persisted_variables).should be true
    end

    describe "data type" do
      describe "empty value" do
        it "should return false when the value is empty" do
          PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "")
          condition = Ext::Condition.new "var1", "=", "5", 'number'

          condition.evaluate?(@project, @contact.persisted_variables).should be false
        end
      end

      describe "nil value" do
        it "should return false when the value is nil" do
          PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: nil)
          condition = Ext::Condition.new "var1", "=", "5", 'number'

          condition.evaluate?(@project, @contact.persisted_variables).should be false
        end
      end

      describe "exact value" do
        it "should not validate data type number and return false even it's match" do
          PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "5")
          persisted_variables = @contact.persisted_variables
          condition = Ext::Condition.new "var1", "=", "5", 'number'

          condition.evaluate?(@project, persisted_variables).should be false
        end
      end

      describe "date time" do
        before(:each) do
          @today = Date.new(2013, 03, 22)
          Date.stub!(:today).and_return(@today)
        end

        it "should today is '22/03/2013'" do
          Date.today.to_string(Date::DEFAULT_FORMAT).should eq '22/03/2013'
        end

        describe "persisted variable value is number" do
          it "should return false" do
            PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "5")
            persisted_variables = @contact.persisted_variables
            condition = Ext::Condition.new "var1", "=", "5", 'day'

            condition.evaluate?(@project, persisted_variables).should be false
          end
        end

        describe "day ago" do
          before(:each) do
            @persisted_variable = PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "20/03/2013")
            @persisted_variables = @contact.persisted_variables
          end

          it "should persisted variable has 1 element" do
            @persisted_variables.length.should eq 1
          end

          it "should first element of persisted value is '20/03/2013'" do
            @persisted_variables.first.value.should eq "20/03/2013"
          end

          it "should return true when persisted variable has value is equal to 2 day ago" do
            condition = Ext::Condition.new "var1", "=", "2", 'day'

            condition.evaluate?(@project, @persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is greater than 2 day ago" do
            condition = Ext::Condition.new "var1", ">", "2", 'day'

            condition.evaluate?(@project, @persisted_variables).should be false
          end

          it "should return true when persisted variable value is greater or equal to 2 day ago" do
            condition = Ext::Condition.new "var1", ">=", "2", 'day'

            condition.evaluate?(@project, @persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is less than 2 day ago" do
            condition = Ext::Condition.new "var1", "<", "2", 'day'

            condition.evaluate?(@project, @persisted_variables).should be false
          end

          it "should return true when persisted variable has value is less or equal to 2 day ago" do
            condition = Ext::Condition.new "var1", "<=", "2", 'day'

            condition.evaluate?(@project, @persisted_variables).should be true
          end
        end

        describe "week ago" do
          before(:each) do
            @persisted_variable = PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "15/03/2013")
            @persisted_variables = @contact.persisted_variables
          end

          it "should persisted variable has 1 element" do
            @persisted_variables.length.should eq 1
          end

          it "should first element of persisted value is '15/03/2013'" do
            @persisted_variables.first.value.should eq "15/03/2013"
          end

          it "should return true when persisted variable has value is equal to 1 week ago" do
            condition = Ext::Condition.new "var1", "=", "1", 'week'

            condition.evaluate?(@project, @persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is greater than 1 week ago" do
            condition = Ext::Condition.new "var1", ">", "1", 'week'

            condition.evaluate?(@project, @persisted_variables).should be false
          end

          it "should return true when persisted variable value is greater or equal to 1 week ago" do
            condition = Ext::Condition.new "var1", ">=", "1", 'week'

            condition.evaluate?(@project, @persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is less than 1 week ago" do
            condition = Ext::Condition.new "var1", "<", "1", 'week'

            condition.evaluate?(@project, @persisted_variables).should be false
          end

          it "should return true when persisted variable has value is less or equal to 1 week ago" do
            condition = Ext::Condition.new "var1", "<=", "1", 'week'

            condition.evaluate?(@project, @persisted_variables).should be true
          end
        end

        describe "month ago" do
          before(:each) do
            @persisted_variable = PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "22/02/2013")
            @persisted_variables = @contact.persisted_variables
          end

          it "should persisted variable has 1 element" do
            @persisted_variables.length.should eq 1
          end

          it "should first element of persisted value is '22/02/2013'" do
            @persisted_variables.first.value.should eq "22/02/2013"
          end

          it "should return true when persisted variable has value is equal to 1 month ago" do
            condition = Ext::Condition.new "var1", "=", "1", 'month'

            condition.evaluate?(@project, @persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is greater than 1 month ago" do
            condition = Ext::Condition.new "var1", ">", "1", 'month'

            condition.evaluate?(@project, @persisted_variables).should be false
          end

          it "should return true when persisted variable value is greater or equal to 1 month ago" do
            condition = Ext::Condition.new "var1", ">=", "1", 'month'

            condition.evaluate?(@project, @persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is less than 1 month ago" do
            condition = Ext::Condition.new "var1", "<", "1", 'month'

            condition.evaluate?(@project, @persisted_variables).should be false
          end

          it "should return true when persisted variable has value is less or equal to 1 month ago" do
            condition = Ext::Condition.new "var1", "<=", "1", 'month'

            condition.evaluate?(@project, @persisted_variables).should be true
          end
        end

        describe "year ago" do
          before(:each) do
            @persisted_variable = PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "22/03/2012")
            @persisted_variables = @contact.persisted_variables
          end

          it "should persisted variable has 1 element" do
            @persisted_variables.length.should eq 1
          end

          it "should first element of persisted value is '22/03/2012'" do
            @persisted_variables.first.value.should eq "22/03/2012"
          end

          it "should return true when persisted variable has value is equal to 1 year ago" do
            condition = Ext::Condition.new "var1", "=", "1", 'year'

            condition.evaluate?(@project, @persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is greater than 1 year ago" do
            condition = Ext::Condition.new "var1", ">", "1", 'year'

            condition.evaluate?(@project, @persisted_variables).should be false
          end

          it "should return true when persisted variable value is greater or equal to 1 year ago" do
            condition = Ext::Condition.new "var1", ">=", "1", 'year'

            condition.evaluate?(@project, @persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is less than 1 year ago" do
            condition = Ext::Condition.new "var1", "<", "1", 'year'

            condition.evaluate?(@project, @persisted_variables).should be false
          end

          it "should return true when persisted variable has value is less or equal to 1 year ago" do
            condition = Ext::Condition.new "var1", "<=", "1", 'year'

            condition.evaluate?(@project, @persisted_variables).should be true
          end
        end
      end
    end
  end

end
