require 'spec_helper'


describe String do 
  describe "#is_contact?" do
    it "should invalid when it is blank" do
      ''.is_contact?.should be_false
    end

    it "should invalid when it has include character" do
      '123ds32'.is_contact?.should be_false
    end

    it "should invalid when it has special character" do
      '1234,009'.is_contact?.should be_false
      '1234@009'.is_contact?.should be_false
      '!1234009'.is_contact?.should be_false
    end

    it "should valid when it has heading space" do
      '    123456'.is_contact?.should be_true
    end

    it "should valid when it has trailing space" do
      '123456   '.is_contact?.should be_true
    end

    it "should valid when it has space in the middle" do
      '123   456'.is_contact?.should be_true
    end

  end
end