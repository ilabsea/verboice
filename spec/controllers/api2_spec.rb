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

describe Api2 do
  context ".allow_hosts" do
    it { expect(Api2.allow_hosts['allow_hosts'].include?("0.0.0.0")).to be true }
    it { expect(Api2.allow_hosts['allow_hosts'].include?("127.0.0.1")).to be true }
  end

  context ".host_allowed?" do
    it { Api2.host_allowed?("0.0.0.0").should be_true }
    it { Api2.host_allowed?("127.0.0.1").should be_true }
    it { Api2.host_allowed?("192.168.1.1").should be_false }
  end  
end