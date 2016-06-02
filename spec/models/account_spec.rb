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
  let(:user) { Account.make role: Account::USER }

  it { should have_many(:projects) }
  it { should have_many(:channels) }
  it { should have_many(:call_logs) }
  it { should have_many(:ext_reminder_groups) }

  describe 'save' do
    it 'generate auth_token for user' do
      super_user.auth_token.empty?.should be false
    end
  end

  describe "#admin?" do
    it { super_user.admin?.should be true }
    it { user.admin?.should be false }
  end

  describe "#user?" do
    it { super_user.user?.should be false }
    it { user.user?.should be true }
  end
  
  describe "#has_access_from?" do
    it { super_user.has_access_from?("192.192.192.192").should be false }
    it { super_user.has_access_from?("127.0.0.1").should be true }
  end
end