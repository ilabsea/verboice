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

describe Account do
  let(:super_user) { Account.make role: Account::ADMIN }

  it { should have_many(:projects) }
  it { should have_many(:channels) }
  it { should have_many(:call_logs) }
  it { should have_many(:ext_reminder_groups) }
  
  describe "admin?" do
    it "should false when it is an admin but there is no host available" do
      Billing.should_receive(:hosts).and_return([])
      
      super_user.admin?("127.0.0.1").should be_false
    end

    it "should false when it is an admin but host doesn't match" do
      Billing.should_receive(:hosts).and_return(["127.0.0.1"])

      super_user.admin?("192.192.192.192").should be_false
    end

    it "should true when it is an admin and host is valid" do
      Billing.should_receive(:hosts).and_return(["127.0.0.1"])
      
      super_user.admin?("127.0.0.1").should be_true
    end

  end
end
