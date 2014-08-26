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

describe Login::Ip do
  before(:each) do
    LoginTracker.make origin_ip: '127.0.0.1', email: 'foo@verboice.org'
    LoginTracker.make origin_ip: '127.0.0.1', email: 'foo@verboice.org'
    LoginTracker.make origin_ip: '127.0.0.1', email: 'foo@verboice.org'
    LoginTracker.make origin_ip: '127.0.0.1', email: 'foo@verboice.org'
    LoginTracker.make origin_ip: '127.0.0.1', email: 'foo@verboice.org'

    LoginTracker.make origin_ip: '127.0.0.1', email: 'bar@verboice.org'
  end

  describe "#reaches_maximum_failed_attempt?" do
    context "email is attacked" do
      ip = Login::Ip.new('127.0.0.1')
      it { ip.reaches_maximum_failed_attempt?('foo@verboice.org').should == true }
    end

    context "email is attacted from difference IP" do
      ip = Login::Ip.new('10.10.10.1')
      it { ip.reaches_maximum_failed_attempt?('foo@verboice.org').should == false }
    end

    context "email is not attacked" do
      ip = Login::Ip.new('127.0.0.1')
      it { ip.reaches_maximum_failed_attempt?('bar@verboice.org').should == false }
    end
  end
  
end
