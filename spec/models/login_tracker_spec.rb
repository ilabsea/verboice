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

describe LoginTracker do
  describe ".disable_previous_failed_history" do
    before(:each) do
      LoginTracker.make origin_ip: '127.0.0.1', email: 'foo@verboice.org'
      LoginTracker.make origin_ip: '127.0.0.1', email: 'foo@verboice.org'
      LoginTracker.make origin_ip: '10.10.10.1', email: 'foo@verboice.org'
      LoginTracker.make origin_ip: '10.10.10.1', email: 'foo@verboice.org'
      LoginTracker.make origin_ip: '10.10.10.1', email: 'foo@verboice.org'

      @from_ip = '127.0.0.1'
      @login_email = 'foo@verboice.org'
    end

    it "should login from localhost" do
      @from_ip.should eq("127.0.0.1")
    end

    it "should login as 'foo@verboice.org'" do
      @login_email.should eq('foo@verboice.org')
    end

    it "should disable previous failed history of the login email" do
      access_ip = Login::Ip.new @from_ip

      expect {  access_ip.log @login_email, LoginTracker::MARKED_AS_SUCCESS }.to change{LoginTracker.active.failed.count}.by(-2)

    end
  end
  
end
